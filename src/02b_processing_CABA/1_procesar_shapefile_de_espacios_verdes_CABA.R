library(sf)
library(igraph)
library(tidyverse)

###########################################################
# Intersección limite CABA con los Espacios Verdes de OSM #
###########################################################


## Cargamos las bases de datos con las que vamos a estar trabajando

areas_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp") 

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(st_crs(areas_verdes))

# Hacemos una interseccion espacial para quedarnos solo con los espacios verdes
# al interior de la Ciudad de Buenos Aires.
areas_verdes_CABA <- areas_verdes %>% 
    st_intersection(CABA_limite) %>% 
    st_collection_extract("POLYGON") %>% 
    st_difference() %>% 
    select(-FID)

## INSPECCION VISUAL
ggplot()+
    geom_sf(data = CABA_limite) + 
    geom_sf(data=areas_verdes_CABA, fill="grey50")+
    theme_void()

# Guardamos
st_write(areas_verdes_CABA, "data/processed/osm/areas_verdes_CABA.shp", delete_dsn = TRUE)
