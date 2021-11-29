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
    geom_sf(data = radios_censales, fill="gray95", color="grey70")+
    geom_sf(data = verdes, aes(fill=cierre))+
    geom_sf(data = barrios, fill=NA, color="black")+
    labs(fill="Espacio")+
    scale_fill_manual(values = c("#ffcd00", "#8F00FF"))+
    theme_void()

# Diferencia (Ponderación)

ggplot()+
    geom_sf(data= radios_censales, fill="gray95", color="grey70")+
    geom_sf(data = verdes, aes(fill="Diferencia"))+
    geom_sf(data = verdes_cuali, fill="#ffcd00")+
    geom_sf(data = barrios, fill=NA, color="black")+
    scale_fill_manual(values = ("Diferencia" = "#8F00FF"))+
    labs(fill="")+
    theme_void()
