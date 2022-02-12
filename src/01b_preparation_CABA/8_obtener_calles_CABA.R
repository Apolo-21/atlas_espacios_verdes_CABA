library(tidyverse)
library(sf)
library(osmdata)

#################################################
# Descargar calles de la Ciudad de Buenos Aires #
#################################################

# Descargamos las calles de la Ciudad de Buenos Aires (CABA).
bbox_caba <- getbb("Ciudad Autónoma de Buenos Aires, Argentina")

calles_caba <- opq(bbox_caba) %>% 
    add_osm_feature(key="highway")
calles_caba <- osmdata_sf(calles_caba)
calles_caba <- calles_caba$osm_lines %>% 
    st_transform(crs=4326)

# Realicemos una inspección visual de nuestros resultados.
mapview::mapview(calles_caba)

# Cargamos el límite de CABA.
caba_limite <- st_read("data/raw/OSM/limite_CABA.shp")

# Y lo intersectamos con las calles para quedarnos solo con aquellas dentro de 
# los límites del distrito.
calles_caba_int <- st_intersection(calles_caba, caba_limite) %>% 
    select(osm_id, name, FID, geometry) %>% 
    st_collection_extract("LINESTRING") %>% 
    st_difference() # Eliminamos las geometrías que se duplican.

# Visualizamos.
mapview::mapview(calles_caba_2)

#guardamos las calles de OSM
st_write(calles_caba_int, "data/raw/osm/callejero.shp", delete_dsn = TRUE, overwrite=TRUE)
