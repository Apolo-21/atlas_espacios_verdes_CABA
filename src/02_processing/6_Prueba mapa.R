library(tidyverse)
library(ggplot)
library(sf)

aglomerados <- st_read("src/04_app/data/aglomerados.geojson")
radios <- st_read("data/processed/mapbox/radios_con_accesibilidad_para_basemap.geojson")
barrios_CABA <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson")

EV_CABA <- st_read("data/processed/osm/areas_verdes_CABA.shp")

radios_completos <- st_read("data/raw/INDEC/radios_eph.json")

bbox_new <- as.numeric(st_bbox(barrios_CABA))


ggplot()+
    geom_sf(data=radios)+
    geom_sf(data=radios_completos)+
    geom_sf(data=radios %>% filter(con_acceso==FALSE), fill="red")+
    geom_sf(data=EV_CABA, fill="darkgreen")+
    scale_x_continuous(limits = c(-58.55, -58.3)) + 
    scale_y_continuous(limits = c(-34.7, -34.52))
    



#no funciona:

st_write(radios, "src/04_app/data/radios_con_accesibilidad.sph", driver="ESRI Shapefile")  # create to a shapefile 

st_write(aglomerados, "src/04_app/data/aglomerados.sph", driver="ESRI Shapefile")
