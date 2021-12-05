# Descargamos el export de todos los datos que tiene OSM para la Argentina, via GEOFABRIK: https://download.geofabrik.de/south-america/argentina.html
# El shapefile procesado fue obtenido de https://download.geofabrik.de/south-america/argentina-latest-free.shp.zip

library(sf)
library(igraph)
library(tidyverse)

## Carga de datos

areas_verdes <- st_read("data/raw/OSM/gis_osm_landuse_a_free_1.shp", 
                        stringsAsFactors = F) %>%
    filter(fclass %in% c("nature_reserve", "park")) %>% 
    select(-code)

# Ante todo, proyección equiareal para una medición precisa de áreas y distancias
areas_verdes <- areas_verdes %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


## Retirar Grandes Parques Nacionales del dataset

# Entre las áreas de categoría "nature_reserve", la mayoría de los polígonos representan 
# Parques Nacionales y diversos territorios protegidos (biomas marinos, de alta montaña, humedales, etc.) 
# que no pueden ser considerados "espacios verdes" en el sentido de opciones cotidianas de recreación 
# para la población urbana. Pese a ello, existen casos dentro de la categoría que si corresponden a la categoría 
# de espacios accesibles de recreación: el Parque Pereyra Iraola, la Reserva Ecológica porteña, etc.


# Se prepara una capa para inspección visual
# check <- areas_verdes %>%
#     filter(fclass == "nature_reserve") %>%
#     mutate(area = as.numeric(st_area(.))) %>%
#     arrange(area)

# Tras la inspección, se categoriza cómo areas verdes con accesibilidad urbana a las de los índices 
# "612333451", "79291703", "255963872", "550726747", "220997430", "229796895", 
# "191347957", "90264216", "7322563", "51185722", "3810531", "3642306", 
# "350935744", "49772911", "5539208", "46945607", "185329483", "10343154"
# Retenemos esos, y descartamos los demas.


keep_ids <- c("612333451", "79291703", "255963872", "550726747", "220997430", "229796895", 
         "191347957", "90264216", "7322563", "51185722", "3810531", "3642306", 
         "350935744", "49772911", "5539208", "46945607", "185329483", "10343154")



areas_verdes <- areas_verdes %>% 
    filter(fclass != "nature_reserve" | osm_id %in% keep_ids) 

# Al 17/01/2018, hay varias reservas que figuran con categoría "park". Estas son:
# 729375334, 725714157, 205645634, 375780730
# "Reserva Natural Humedal Caleta Olivia", "Reserva Natural El Destino", 
# "Reserva Natural", "Reserva natural Abayubá"

areas_verdes <- areas_verdes %>% 
    filter(!(osm_id %in% c(729375334, 725714157, 205645634, 375780730))) 


## Retirar vias de circulación, boulevares, etc.

# Existen una gran cantidad de calles, que por error o por tener canteros o areas parquizadas, 
# figuran con la categoría "park".
# Podemos detectar calles y otros elementos estrechos y largos comparando su área con su perímetro.
# Tras inspección visual y fijamos el número mágico a 5, para no perder algunas plazas de forma alargada
# (como ésta en Rosario, osm_id 923228155)

umbral_area_perimetro <- 5

areas_verdes <- areas_verdes %>% 
    filter((as.numeric(st_area(.)) / as.numeric(lwgeom::st_perimeter(.))) > umbral_area_perimetro)


## Combinar las áreas que estan muy próximas entre si
# (a menos de 10m en este caso).
# Con eso unificamos predios separados por alguna via de circulación interna, 
# asi podemos considerar su tamaño total luego

# Generamos un buffer en torno a los polígonos, e identificamos sus solapamientos.
# La ideas es asignar un id de grupo que asocie a los buffers que forman parte de una 
# misma "cadena" de poligonos que se solapan entre si, demarcando el area de los parques que consideramos combinados
# para esto tenemos que armar un grafo de poligonos: 
# si A toca a B, y B toca a C, entonces A y C son parte de un mismo grupo
# Vease https://gis.stackexchange.com/a/323067/59568

umbral_de_proximidad <- 5

buffers <- st_buffer(areas_verdes, umbral_de_proximidad) 

solapamientos = st_intersects(buffers, buffers)

# Armamos el grafo a partir de la matriz de adyacencia generada por st_intersects()
grafo <- graph_from_adj_list(solapamientos)

# Le asignamos a cada buffer el ID de grupo/cluster al que pertenece 
buffers <- buffers %>% 
    mutate(cluster_id = components(grafo)$membership) %>% 
    group_by(cluster_id) %>% 
    summarise()

# Le asignamos a cada espacio verde el grupo de proximidad al que pertenece
areas_verdes <- areas_verdes %>% 
    st_join(buffers) 

summary(areas_verdes$cluster_id)

## Retenemos sólo los clusters cuya area combinada supera un umbral de corte.

# descartamos los menores a 1000 m2 (más pequeños que una plazoleta, aproximadamente)
umbral_descarte_m2 <- 1000

areas_verdes <- areas_verdes %>% 
    mutate(area_m2 = as.numeric(st_area(.))) 

areas_verdes <- areas_verdes %>% 
    group_by(cluster_id) %>% 
    filter(sum(area_m2) > umbral_descarte_m2) # Descartamos áreas menores al umbral


# Guardamos
st_write(areas_verdes, "data/processed/osm/areas_verdes_urbanas_argentina.shp", delete_dsn = TRUE)
