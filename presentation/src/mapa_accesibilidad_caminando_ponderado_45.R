library(sf)
library(tidyverse)
library(ggplot2)

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    st_difference()

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>% 
    st_transform(4326)
comunas <- comunas %>% 
    st_intersection(CABA_limite)

EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326)


#accesbilidad caminando (ponderada):
accesibilidad <- st_read("data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA.shp")

# % sobre deficitario s/area de CABA
accesibilidad_m2_total <- accesibilidad %>% 
    filter(situacn=="sin_acceso")

sum(st_area(accesibilidad_m2_total))/sum(st_area(CABA_limite))*100


# Accesibilidad base (10 minutos) reprocesada:
accesibilidad_base <- st_read("data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA_orginal.shp")

# % sobre deficitario s/area de CABA
accesibilidad_base_m2_total <- accesibilidad_base %>% 
    filter(situacn=="sin_acceso")
sum(st_area(accesibilidad_base_m2_total))/sum(st_area(CABA_limite))*100



ggplot()+
#    geom_sf(data=accesibilidad %>% filter(situacn=="sin_acceso"), aes(fill="Nuevo índice"), color=NA)+
    geom_sf(data=accesibilidad_base %>% 
                filter(situacn=="sin_acceso"), aes(fill="Índice original"), color=NA)+
    geom_sf(data=EV, fill="#69b166", size=.1)+
    geom_sf(data=radios_CABA, fill=NA, fill="grey96", color="grey66")+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.1)+
    scale_fill_manual(values = c("Índice original" = "grey10", "Índice nuevo"= "grey60"))+
    labs(fill="Radios censales sin acceso a EV")+
    theme_void()

    