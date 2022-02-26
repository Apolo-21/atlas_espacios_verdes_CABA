library(tidyverse)
library(sf)
library(leaflet)


####################################################################
# Relevamiento de espacios verdes: Cerrados vs Abiertos - 2º Parte #
####################################################################

# El siguiente script busca completar el proceso de relevamiento de los espacios
# verdes (EV) de la Ciudad de Buenos Aires (CABA) iniciado en el script anterior.

# IMPORTANTE: El relevamiento fue realizado entre el 1 y 12 de octubre de 2021. 
# A la fecha actual, es posible que el dataset de espacios relevados se encuentre 
# desactualizado y/o que diverja de los resultados arrojados por OSM.

# Cargamos los espacios verdes relevados.
ev_relevado <- read.csv("data/processed/GCABA/EV/Ev-completo.csv",
                                stringsAsFactors = TRUE, 
                                encoding = "UTF-8") 

#--Reserva Ecológica Costanera Sur----------------------------------------------

# A partir del primer entrecruzamiento de datos entre la base de datos de OSM y 
# la del MEPHU, la Reserva Ecológica fue catalogado como un espacio "Abierto". Sin
# embargo, a partir del relevamiento virtual realizado posteriormente, se identificó
# que la misma posee horarios de apertura y cierre determinados, por lo que se decidió
# cambiar su condición a "cerrable".

ev_relevado <- ev_relevado %>% 
    mutate(cierre = if_else(osm_id == "10343154", "Cerrable", as.character(cierre)))

#-------------------------------------------------------------------------------

summary(as.factor(ev_relevado$cierre)) # Se encuentran enrejados un tercio (173) de los espacios verdes de la CABA.

# Volvamosle a cargar las geometrías de nuestros espacios verdes.
areas_verdes_caba <- st_read("data/processed/osm/areas_verdes_urbanas_caba.shp") %>% 
    st_transform(st_crs(4326)) %>% 
    select(osm_id) %>% 
    mutate(osm_id=as.numeric(osm_id))

areas_verdes_caba <- areas_verdes_caba %>% 
    left_join(ev_relevado, by="osm_id")

# A partir del relevamiento, se reconocieron varios espacios que, pese a ser catalogados
# como públicos y verdes, no lo eran realmente, ellos son:
areas_verdes_caba %>% 
    filter(!is.na(comentarios)) %>% 
    as_tibble()

# Visualización.
areas_verdes_caba %>% 
    filter(!is.na(comentarios)) %>% 
    leaflet() %>% 
    addPolygons(popup = ~paste("<br><b>NOMBRE:</b>", name,
                               "<br><b>ID OSM:</b>", osm_id,
                               "<br><b>CONDICIÓN:</b>", comentarios)) %>% 
    addTiles()

# Vamos a filtrarlos de nuestro dataset.
areas_verdes_caba <- areas_verdes_caba %>% 
    filter(is.na(comentarios))

summary(as.factor(areas_verdes_caba$cierre)) # Se encuentran enrejados un cuarto (159) de los espacios verdes de la CABA.

# En vistas de esta última modificación. En la CABA existen 653 Espacios Verdes 
# Públicos que cumplen con los parámetros de calidad antes expuestos. De ese total,
# 158 se encuentran enrejados o poseen horarios determinados de vista, lo que es
# equivalente al 32% del total.

cccc <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

# Guardamos los datos procesados.
st_write(areas_verdes_caba, "data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp", delete_layer = T)


# Inspección visual con las nuevas categorías de EV ----------------------------

leaflet() %>% 
    addPolygons(data= areas_verdes_caba,
                popup = ~ paste("<br><b>NOMBRE:</b>", name,
                                "<br><b>ID OSM:</b>", osm_id,
                                "<br><b>CLUSTER ID:</b>", cluster_id,
                                "<br><b>ENREJADO:</b>", cierre,
                                "<br><b>ESCALA:</b>", escala,
                                "<br><b>JUEGOS:</b>", patio_de_j,
                                "<br><b>CANIL:</b>", canil,
                                "<br><b>POSTA AERÓBICA:</b>", posta_aero,
                                "<br><b>CLASIFICACIÓN:</b>", clasificac)) %>% 
    addTiles()

