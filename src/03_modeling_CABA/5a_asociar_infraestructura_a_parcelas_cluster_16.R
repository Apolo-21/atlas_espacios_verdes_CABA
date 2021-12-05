library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

################################################################################
# Asociar infraestructura potencialmente reconvertible al parcelario
################################################################################

# Cargamos las bases de datos

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_difference() 

proj <- st_crs(CABA_limite)

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(proj)

estacionamientos <- st_read("data/processed/osm/estacionamientos_relevados.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=proj)

estacionamientos_aptos <- estacionamientos %>% 
    dplyr::filter(apto =="si") # registrados como aptos en el relevamiento visual

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(proj)

manzanas <- st_read("data/raw/GCABA/Manzanas/manzanas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(radios_cluster_16)

# Nos quedamos exclusivamente con las manzanas internas a nuestra mancha deficitaria
parcelas <- st_read("data/raw/GCABA/Parcelario/parcelario_cluster_16.shp") %>% 
    st_transform(crs=proj) %>% 
    dplyr:: select(SMP, SUP_PARCEL, TOT_POB)

#_______________________________________________________________________________

## ESTACIONAMIENTOS POR PARCELAS

# encontramos las parcelas que registra al menos un punto de estacionamiento, muchas tienen putnos repetidos (de OSM)

estacionamientos_por_parcela <- estacionamientos_aptos %>% 
    st_difference() %>% 
    st_join(parcelas) %>% 
    drop_na(SMP) %>% 
    group_by(SMP) %>% 
    summarise() %>% 
    as.data.frame() %>% 
    dplyr:: select(SMP) %>% 
    mutate(PARKING=1)

parcelas_parking <- left_join(parcelas, estacionamientos_por_parcela, by="SMP")

# Inspección visual


#radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
#    st_transform(proj)

ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas, fill="grey85", color="grey40", size=.8)+
    geom_sf(data=parcelas_parking %>% filter(PARKING==1), fill="#8F00FF")+
#    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()

# guardamos
st_write(parcelas_parking, "data/processed/GCABA/parcelas_potenciales/parcelas_potenciales_cluster_16.shp", delete_dsn = TRUE)


#_______________________________________________________________________________
    
# TERRENOS BALDIOS Y DEPOSITOS

# repetimos el procedimiento de union de puntos con parcelas, pero en este caso 
# pasandole el dataset de Properati de lotes vacantes y depositos
    
lotes_vacantes <- st_read("data/raw/PROPERATI/properati_lotes_y_depositos.shp") %>% #lotes vacantes y depositos de Properati
    st_transform(crs=proj)
    
lotes_vacantes_por_parcela <- lotes_vacantes %>% 
    st_difference() %>% 
    st_join(parcelas) %>% 
    drop_na(SMP) %>% 
    group_by(SMP) %>% 
    summarise() %>% 
    as.data.frame() %>% 
    dplyr:: select(SMP) %>% 
    mutate(VACANTE=1)

parcelas_vacantes_y_parking <- left_join(parcelas, lotes_vacantes_por_parcela, by="SMP")

# le unimos la columna de parking
parcelas_vacantes_y_parking <- left_join(parcelas_vacantes_y_parking, estacionamientos_por_parcela, by="SMP")

# Inspeccion visual
ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas, fill="grey85", color="grey40", size=.8)+
    geom_sf(data=parcelas_vacantes_y_parking %>% filter(VACANTE==1 | PARKING==1), fill="#8F00FF")+
    #    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()
    
# guardamos
st_write(parcelas_vacantes_y_parking, "data/processed/GCABA/parcelas_potenciales/parcelas_vacantes_cluster_16.shp", delete_dsn = TRUE)
    

