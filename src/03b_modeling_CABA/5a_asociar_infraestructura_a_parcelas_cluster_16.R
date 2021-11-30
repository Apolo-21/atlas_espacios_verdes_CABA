library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_difference() 

proj <- st_crs(CABA_limite)

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(proj)

estacionamientos <- st_read("data/processed/osm/estacionamientos_relevados.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=proj)

estacionamientos_aptos <- estacionamientos %>% 
    dplyr::filter(apto=="si")

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(proj)


# Nos quedamos exclusivamente con las manzanas internas a nuestra mancha deficitaria

parcelas <- st_read("data/raw/GCABA/Parcelario/parcelario_cluster_16.shp") %>% 
    st_transform(crs=proj) %>% 
    dplyr:: select(SMP, SUP_PARCEL, TOT_POB)

# encontramos las parcelas que registra al menos un punto de estacionamiento, muchas tienen putnos repetidos
estacionamientos_por_parcela <- estacionamientos_aptos %>% 
    st_difference() %>% 
    st_join(parcelas) %>% 
    drop_na(SMP) %>% 
    group_by(SMP) %>% 
    summarise() %>% 
    as.data.frame() %>% 
    dplyr:: select(SMP) %>% 
    mutate(PARKING=1)

    
parcelas <- left_join(parcelas, estacionamientos_por_parcela, by="SMP")

# Inspección visual
manzanas <- st_read("data/raw/GCABA/Manzanas/manzanas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(radios_cluster_16)

#radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
#    st_transform(proj)

ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas, fill="grey85", color="grey40", size=.8)+
    geom_sf(data=parcelas %>% filter(PARKING==1), fill="#8F00FF")+
#    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()


st_write(parcelas, "data/processed/GCABA/parcelas_potenciales/parcelas_potenciales_cluster_16.shp", delete_dsn = TRUE)


