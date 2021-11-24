library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_difference() 

proj <- st_crs(CABA_limite)

radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
    st_transform(proj)


estacionamientos <- st_read("data/processed/osm/estacionamientos_relevados.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=proj) %>% 
    select(osm_id)


radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(proj)


# Nos quedamos exclusivamente con las manzanas internas a nuestra mancha deficitaria

manzanas <- st_read("data/raw/GCABA/Manzanas/Manzanas.geojson") %>% 
    st_transform(crs=proj) %>% 
    select(SM, FeatId1) %>% 
    st_intersection(radios_cluster_12)

# encontramos las parcelas que registra al menos un punto de estacionamiento, muchas tienen putnos repetidos
estacionamientos_por_manzanas <- estacionamientos %>% 
    st_difference() %>% 
    st_join(manzanas) %>% 
    drop_na(SM) %>% 
    group_by(SM) %>% 
    summarise() %>% 
    as.data.frame() %>% 
    select(SM) %>% 
    mutate(PARKING=1)

    
manzanas <- left_join(manzanas, estacionamientos_por_manzanas, by="SM")

# Inspección visual
ggplot()+
    geom_sf(data=radios_cluster_12, fill=NA, fill="grey96", color="grey66")+
    geom_sf(data=manzanas, fill="grey90")+
    geom_sf(data=manzanas %>% filter(PARKING==1), fill="red")+
    theme_void()


st_write(manzanas, "data/processed/GCABA/manzanas_con_parcelas_potenciales/manzanas_potenciales_cluster_12.shp", delete_dsn = TRUE)


