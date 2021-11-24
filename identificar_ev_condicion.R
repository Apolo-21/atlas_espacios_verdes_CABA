library(tidyverse)
library(sf)
library(leaflet)
library(ggmap)
library(osrm)

verde <- read_sf("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

summary(as.factor(verde$fencing)) #Que onda estos sin Cluster ID?

verde_filt <- verde %>% 
    st_transform(4326) %>% 
    filter(is.na(fencing))

cerrados <- verde %>% 
    filter(fencing=="Cerrable")

relevados <- verde %>% 
    filter(is.na(verde$fch_rlv))

leaflet(verde_filt) %>%
    addTiles() %>% 
    addPolygons(label=paste("ID", verde_filt$osm_id,
                            "Cluster", verde_filt$clstr_d))

ggplot(verde) +
    geom_sf(aes(fill=fencing))+
    labs(fill ="Espacio\nPúblico")+
    theme_void()

bbox_caba <- as.numeric(st_bbox(verde))

mapacaba <- get_stamenmap(bbox = bbox_caba,
                                 maptype = "toner-lite",
                                 zoom=13)

ggmap(mapacaba) +
    geom_sf(data=verde, aes(fill=fencing), inherit.aes = F)+
    labs(fill ="Espacio\nPúblico")+
    theme_void()

