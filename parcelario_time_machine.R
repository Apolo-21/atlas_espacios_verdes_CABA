library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE)

parcelario <- st_read("data/raw/GCABA/Parcelario/210903_Parcelario_SSREGIC_DGAUR.shp")

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE)

cluster16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(st_crs(4326))

cluster12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
    st_transform(crs = 4326)

parcelario <-  parcelario %>% 
    st_transform(st_crs(4326))

radios_CABA <- radios_CABA %>% 
    st_transform(st_crs(parcelario))

ggplot()+
    geom_sf(data = cluster16, color="black")+
    geom_sf(data = parcelario_16, fill=NA, color="red")+
    geom_sf(data = estacionamientos, size=0.5)
    
    
st_crs(parcelario)

geo_poligonos <- st_geometry(parcelario)

nueva_geo <- geo_poligonos + c(-0.0006, 0.00045)

parcelario <- parcelario %>% 
    st_set_geometry(nueva_geo)

parcelario <- parcelario %>% 
    st_set_crs(4326)

parcelario_16 <- parcelario %>% 
    st_intersection(cluster16) %>% 
    unique()

estacionamientos <- st_read("data/processed/osm/estacionamientos_relevados.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=4326)

library(leaflet)

leaflet(parcelario_16) %>% 
    addTiles() %>% 
    addPolygons()

write_sf(parcelario_16, "parcelario_cluster_16.shp")
    
# Cluster 12

geo_poligonos <- st_geometry(parcelario)

nueva_geo <- geo_poligonos + c(-0.0006, 0.00043)

parcelario <- parcelario %>% 
    st_set_geometry(nueva_geo)

parcelario <- parcelario %>% 
    st_set_crs(4326)

parcelario_12 <- parcelario %>% 
    st_intersection(cluster12) %>% 
    unique()

ggplot()+
    geom_sf(data = cluster12, color="black")+
    geom_sf(data = parcelario_12, fill=NA, color="red")

leaflet(parcelario_12) %>% 
    addTiles() %>% 
    addPolygons()

write_sf(parcelario_12, "parcelario_cluster_12.shp")

