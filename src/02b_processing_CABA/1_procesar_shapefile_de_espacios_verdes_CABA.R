library(sf)
library(igraph)
library(tidyverse)

## Carga de datos de espacios verdes ya procesador a nivel pais

areas_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp")

#Cargamos el límite geográfico de la capital y le asgnamos el mismo sistema de coordenadas que nuestro dataset de espacios verdes.
CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(st_crs(areas_verdes))

#Intersectamos nuestros datasets, y solo nos quedamos con las áreas verdes pertenecientes a la Ciudad de Buenos Aires.

areas_verdes_CABA <- areas_verdes %>% 
    st_intersection(CABA_limite) %>% 
    st_collection_extract("POLYGON") %>% 
    st_difference() %>% 
    select(-FID)

## INSPECCION VISUAL
ggplot()+
    geom_sf(data = CABA_limite) + 
    geom_sf(data=areas_verdes_CABA, fill="grey50")+
    theme_minimal()

# Guardamos
st_write(areas_verdes_CABA, "data/processed/osm/areas_verdes_CABA.shp", delete_dsn = TRUE)
