library(sf)
library(igraph)
library(tidyverse)
library(mapview)

####################################
# Procesamiento de espacios verdes #
####################################

## Cargamos los datos de espacios verdes extraidos de geofabrick.de.
areas_verdes <- st_read("data/raw/OSM/gis_osm_landuse_a_free_1.shp", 
                        stringsAsFactors = F) %>%
    # Filtramos por aquellos espacios verdes que entran en la categoría "reserva natural" y "parque".
    filter(fclass %in% c("nature_reserve", "park")) %>% 
    select(-code)

# Aplicamos una proyección equiareal para una medición precisa de áreas y distancias
areas_verdes <- areas_verdes %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


## Selección de espacios verdes de la Ciudad de Buenos Aires (CABA).

# Vamos a quedarnos solo con los espacios verdes de la CABA. Para ello, vamos a 
# cargar los límites de la misma.
caba_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(st_crs(areas_verdes))

# Intersectamos ambos datasets.
areas_verdes_caba <- areas_verdes %>% 
    st_intersection(caba_limite) %>% 
    st_collection_extract("POLYGON") %>% 
    st_difference() %>% 
    select(-FID)

nrow(areas_verdes_caba)
# De acuerdo con los datos de OSM, al 2 de Febrero, hay registrados 1116 espacios
# verdes. Veamoslos:
mapview(areas_verdes_caba)


## Retirar vias de circulación, boulevares, etc.

# Existen una gran cantidad de calles que, por error o por tener canteros o áreas
# parquizadas, figuran con la categoría "park". En esos casos, podemos detectar
# calles y otros elementos estrechos y largos comparando su área con su perímetro.
# Tras una inspección visual, fijamos el número mágico a 5, para no perder algunas
# plazas de forma alargada

umbral_area_perimetro <- 5

areas_verdes_caba <- areas_verdes_caba %>% 
    filter((as.numeric(st_area(.)) / as.numeric(lwgeom::st_perimeter(.))) > umbral_area_perimetro)

nrow(areas_verdes_caba)
# Al aplicar este filtro, el número de espacios verdes deciende a 757, lo que significa
# que 359, casi un tercio de los registros, fueron eliminados. Observemos aqullos
# que quedaron:
mapview(areas_verdes_caba)

## Combinar aquellas áreas que estan muy próximas entre si (a menos de 10m).

# Este ejercicio busca unificar aquellos predios separados por alguna via de circulación
# interna, con el fin de considerar su tamaño total de manera agegada. Para ello,
# generamos un buffer en torno a los polígonos, e identificamos sus solapamientos.
# La ideas es asignar un id de grupo que asocie a los buffers que forman parte de
# una misma "cadena" de poligonos que se solapan entre si, demarcando el area de
# los parques que consideramos combinados. Para esto, tenemos que armar un grafo
# de poligonos: si A toca a B, y B toca a C, entonces A y C son parte de un mismo grupo.
# Vease https://gis.stackexchange.com/a/323067/59568

umbral_de_proximidad <- 5

buffers <- st_buffer(areas_verdes_caba, umbral_de_proximidad) 

solapamientos = st_intersects(buffers, buffers)

# Armamos el grafo a partir de la matriz de adyacencia generada por st_intersects().
grafo <- graph_from_adj_list(solapamientos)

# Le asignamos a cada buffer el ID de grupo/cluster al que pertenece.
buffers <- buffers %>% 
    mutate(cluster_id = components(grafo)$membership) %>% 
    group_by(cluster_id) %>% 
    summarise()

# Le asignamos a cada espacio verde el grupo de proximidad al que pertenece.
areas_verdes_caba <- areas_verdes_caba %>% 
    st_join(buffers) 

summary(areas_verdes_caba$cluster_id)


## Retenemos sólo los clusters cuya area combinada supera un umbral de corte.

# descartamos los menores a 1000 m2 (más pequeños que una plazoleta, aproximadamente)
umbral_descarte_m2 <- 1000

areas_verdes_caba <- areas_verdes_caba %>% 
    mutate(area_m2 = as.numeric(st_area(.))) 

areas_verdes_caba <- areas_verdes_caba  %>% 
    group_by(cluster_id) %>% 
    filter(sum(area_m2) > umbral_descarte_m2) # Descartamos áreas menores al umbral

nrow(areas_verdes_caba)
# Una vez aplicado este filtro, observamos que el numero total de espacios verdes
# públicos relevantes en la Ciudad pasó a 669, por lo que, aproximadamente, un 40%
# de los registrados por OSM fue descartado. Veamos la selección definitiva:
mapview(areas_verdes_caba)


#-------------------------------------------------------------------------------
# CURIOSAMENTE, SI SEGUIMOS CON ESTE CAMINO ALTERNATIVO, TENEMOS 1 EV MENOS QUE
# POR EL CAMINO ORIGINAL Y 6 MENOS QUE EN EL INFORME.
# El EV QUE DESCARTA NO ES UN EV.
#-------------------------------------------------------------------------------


# ACLARACIÓN: El Atlas de Espacios Verdes de Argentina de la Fundación Bunge & Born
# introduce un filtro adicional sobre las reservas naturales, con el objetivo de 
# descartar aquellas que no tiene un caracter urbano. Sin embargo, a los fines de 
# este trabajo, no fue necesario reproducir el mismo dadas las características de 
# las reservas porteñas. No obstante, el mismo puede resultar de utilidad si se
# busca replicar la metodología en otra ciudad del país. Para más información, ver:
# https://github.com/bitsandbricks/atlas_espacios_verdes


# Guardamos
st_write(areas_verdes_caba, "data/processed/osm/areas_verdes_urbanas_caba_alternativa.shp", delete_dsn = TRUE)
