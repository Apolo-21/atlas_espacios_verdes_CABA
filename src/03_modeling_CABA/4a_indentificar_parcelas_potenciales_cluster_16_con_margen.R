library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

################################################################################
# Identificar parcelas potenciales en el cluster 16, por tamaño y aspecto
################################################################################

# Cargamos las bases de datos

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(crs=4326)

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326)

parcelario <- st_read("data/raw/GCABA/Parcelario/parcelario_cluster_16.shp") %>% 
    st_transform(crs=4326)

manzanas <- st_read("data/raw/GCABA/Manzanas/manzanas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(radios_cluster_16)

#_______________________________________________________________________________

# Veamos cuántas parcelas cumplen con los requisitos de EEVV para su potencial trasformación,
# solo dentro del cluster 16 y teniendo en cuenta el margen de cobertura añadido

umbral_area <- 5000
umbral_area_perimetro <- 5

parcelario_filter <- parcelario %>% 
    mutate(area=as.numeric(st_area(.)),
           perimetro=as.numeric(st_length(.))) %>% 
    filter(area>=5000,
           area/perimetro>5)

#_______________________________________________________________________________

# Inspección visual

#radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
#    st_transform(crs=4326)

ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas, fill="grey85", color="grey40", size=.8)+
    geom_sf(data=parcelario_filter, fill="#8F00FF")+
#    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()

# Al momento de relevar las parcelas potenciales que surgen de este analisis, vemos que muchas de ellas 
# corresponden a grandes edificios institucionales o patrimoniales

# Dadas las caracteristicas del tejido del area central, procederemos a hacer le analisis para la incorporacion
# de pequeños espacios verdes (acupuntura urbana) donde hoy hay infraestructura gris


# Guardamos las parcelas
st_write(parcelario_filter, "data/processed/GCABA/parcelas_potenciales/parcelas_pontenciales_mas_de_5000_cluster_16.shp", delete_dsn = TRUE)

