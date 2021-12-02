library(tidyverse)
library(sf)
library(osmdata)

################################################################################
#Descargar calles de la Ciudad
################################################################################

# descargamos las calles de la Ciudad de Buenos Aires.
bbox_CABA <- getbb("Ciudad Autónoma de Buenos Aires, Argentina")

calles_CABA <- opq(bbox_CABA) %>% 
    add_osm_feature(key="highway")
calles_CABA <- osmdata_sf(calles_CABA)
calles_CABA <- calles_CABA$osm_lines %>% 
    st_transform(crs=4326)

# Realicemos una inspección visual de nuestros resultados.
ggplot()+
    geom_sf(data=calles_CABA)+
    theme_minimal()


# obtenemos el límite de CABA y lo intersectamos con las calles

CABA_limite <- st_read("data/raw/OSM/limite_CABA.shp")

calles_CABA_int <- st_intersection(calles_CABA, CABA_limite)
calles_CABA_int <- calles_CABA_int %>% 
    select(osm_id, name, FID, geometry)

calles_CABA_2 <- st_collection_extract(calles_CABA_int, "LINESTRING")

ggplot()+
    geom_sf(data=calles_CABA_2)+
    theme_minimal()

#guardamos las calles de OSM
st_write(calles_CABA_2, "data/raw/osm/callejero.shp", delete_dsn = TRUE, overwrite=TRUE)
