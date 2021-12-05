library(tidyverse)
library(osmdata)
library(sf)

###########################################################
# Descargar estacionamientos de la Ciudad cargados en OSM #
###########################################################

# Descargamos los estacionamientos comerciales de OSM.

bbox_CABA <- getbb("Ciudad Autónoma de Buenos Aires, Argentina")

# Descargamos los parkings que aparecen tanto en la categoria building como amenity

parking_CABA_OSM <- opq (bbox_CABA) %>%
    add_osm_features(features = c ("\"building\"=\"parking\"",
                                   "\"amenity\"=\"parking\""))

parking_CABA_OSM <- osmdata_sf(parking_CABA_OSM)
parking_CABA_OSM <- parking_CABA_OSM$osm_points
parking_CABA_OSM <- st_transform(parking_CABA_OSM, crs=4326)

# Nos quedamos solo con aquellos al interior de la Ciudad Autónoma de Buenos Aires, hacienodo una interserccion espacial.

CABA_limite <- st_read("data/raw/osm/limite_CABA.shp")

parking_CABA_OSM <- st_intersection(parking_CABA_OSM, CABA_limite) %>% 
    unique()

# Inspeccion visual
ggplot()+
    geom_sf(data=CABA_limite, fill="grey90")+
    geom_sf(data=parking_CABA_OSM, color="red")+
    theme_void()

# Guardamos nuestra capa espacial.
st_write(parking_CABA_OSM, "data/raw/osm/parking_CABA.shp", delete_dsn = TRUE)
