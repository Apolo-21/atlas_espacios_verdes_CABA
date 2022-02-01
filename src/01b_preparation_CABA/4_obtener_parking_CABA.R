library(tidyverse)
library(osmdata)
library(sf)

###########################################################
# Descargar estacionamientos de la Ciudad de Buenos Aires #
###########################################################

# Descargamos los estacionamientos comerciales desde OSM.
bbox_caba <- getbb("Ciudad Autónoma de Buenos Aires, Argentina")

# Descargamos los parkings que aparecen tanto en la categoria "building" como "amenity".
parking_caba_OSM <- opq (bbox_caba) %>%
    add_osm_features(features = c ("\"building\"=\"parking\"",
                                   "\"amenity\"=\"parking\""))

parking_caba_OSM <- osmdata_sf(parking_caba_OSM)
parking_caba_OSM <- parking_caba_OSM$osm_points
parking_caba_OSM <- st_transform(parking_caba_OSM, crs=4326)

# Nos quedamos solo con aquellos al interior de la Ciudad Autónoma de Buenos Aires,
# Para ello, intersectamos nuestro dataset con el límite geográfico de la Ciudad.
caba_limite <- st_read("data/raw/osm/limite_CABA.shp")

parking_caba_OSM <- st_intersection(parking_caba_OSM, caba_limite)  %>%
    select(osm_id, name, FID, geometry) %>% 
    st_difference()

# Inspeccion visual.
ggplot()+
    geom_sf(data=caba_limite, fill="grey90")+
    geom_sf(data=parking_caba_OSM, color="red")+
    theme_void()

# Guardamos nuestra capa espacial.
st_write(parking_caba_OSM, "data/raw/osm/parking_CABA.shp", delete_dsn = TRUE)
