library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(4326)

estacionamientos <- st_read("data/processed/osm/estacionamientos_relevados.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=4326) %>% 
    dplyr:: select(osm_id)

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    st_difference()

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>% 
    st_transform(4326)
comunas <- comunas %>% 
    st_intersection(CABA_limite)

EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
    st_transform(4326)

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326)


# Nos quedamos exclusivamente con las parcelas internas a nuestra mancha deficitaria

parcelario <- st_read("data/raw/GCABA/Parcelario/210903_Parcelario_SSREGIC_DGAUR.shp") %>% 
    st_transform(crs=4326) %>% 
    dplyr:: select(SMP) %>% 
    st_intersection(radios_cluster_16)

# encontramos las parcelas que registra al menos un punto de estacionamiento, muchas tienen putnos repetidos
estacionamientos_por_parcela <- estacionamientos %>% 
    st_difference() %>% 
    st_join(parcelario) %>% 
    drop_na(SMP) %>% 
    group_by(SMP) %>% 
    summarise() %>% 
    as.data.frame() %>% 
    dplyr:: select(SMP) %>% 
    mutate(PARKING=1)

    
parcelario <- left_join(parcelario, estacionamientos_por_parcela, by="SMP")

# Inspección visual
ggplot()+
    geom_sf(data=radios_cluster_16, fill=NA, fill="grey96", color="grey66")+
    geom_sf(data=parcelario, fill="grey90")+
    geom_sf(data=parcelario %>% filter(PARKING==1), fill="red")+
    theme_void()


st_write(parcelario, "data/processed/GCABA/parcelas_potenciales/estacionamientos_por_parcela.shp",delete_dsn = TRUE)


