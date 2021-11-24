library(tidyverse)
library(sf)

#################################
# Espacios Cerrados vs Abiertos #
#################################

###### Datasets ######
# Radios censales.
radios_censales <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(4326)

# Barrios.
barrios <- read_sf("data/raw/GCABA/Barrios/barrios.shp")

# Espacios verdes cualificados.
verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

# Espacios verdes ponderados.
verdes_cuali <- st_read("data/processed/GCABA/EV/espacios-verdes-ponderados.shp")

#### Visualizaciones ####
# EV Cerrado vs Abierto

ggplot()+
    geom_sf(data = radios_censales, fill="grey96", color="grey66")+
    geom_sf(data = verdes, aes(fill=cierre))+
    geom_sf(data = barrios, fill=NA, color="black")+
    labs(fill="Espacio")+
    theme_void()


# Diferencia (Ponderación)

ggplot()+
    geom_sf(data= radios_censales, fill="grey96", color="grey66")+
    geom_sf(data = verdes, aes(fill="Diferencia"))+
    geom_sf(data = verdes_cuali, fill="#69b166")+
    geom_sf(data = barrios, fill=NA, color="black")+
    scale_fill_manual(values = ("Diferencia" = "red"))+
    labs(fill="")+
    theme_void()
