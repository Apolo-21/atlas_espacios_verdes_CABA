library(sf)
library(tidyverse)

#####################################################################
# Procesamiento de los espacios verdes de la Ciudad de Buenos Aires #
#####################################################################

#-------------------------------------------------------------------------------
# ESTE SCRIPT LO PODRÏAMOS BORRAR SI TOMAMOS EL CAMINO ALTERNATIVO PROPUESTO
#-------------------------------------------------------------------------------

## Cargamos las bases de datos con las que vamos a trabajar en este script:
# 1. Las áreas verdes del país
areas_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp") 

# 2. El límite de la Ciudad de Buenos Aires (CABA)
caba_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(st_crs(areas_verdes))

# Hacemos una interseccion espacial entre ambos para quedarnos solo con los espacios
# verdes al interior de la CABA.
areas_verdes_caba<- areas_verdes %>% 
    st_intersection(caba_limite) %>% 
    st_collection_extract("POLYGON") %>% 
    st_difference() %>% 
    select(-FID)

# Inspección visual.
ggplot()+
    geom_sf(data = caba_limite) + 
    geom_sf(data=areas_verdes_CABA, fill="grey50")+
    theme_void()

# Guardamos
st_write(areas_verdes_caba, "data/processed/osm/areas_verdes_CABA.shp", delete_dsn = TRUE)
