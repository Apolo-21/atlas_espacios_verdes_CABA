library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(crs=4326)

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326)

# Identificaremos parcelas que cumplan con nuestros criterios (tamaños y relación area/perímetro) 
# como posibles localizaciones de nuevos EEVV dentro de nuestra área deficitaria

parcelario <- st_read("data/raw/GCABA/Parcelario/parcelario_cluster_16.shp") %>% 
    st_transform(crs=4326)

# Veamos cuántas parcelas cumplen con los requisitos de EEVV para su trasformación 
# Muy bien, teniendo las parcelas de la marcha deficitaria, vamos a quedarnos solamente con las parcelas que 
# superen el tamaño mínimo y la relación de aspecto

umbral_area <- 5000
umbral_area_perimetro <- 5

parcelario_filter <- parcelario %>% 
    mutate(area=as.numeric(st_area(.)),
           perimetro=as.numeric(st_length(.))) %>% 
    filter(area>=5000,
           area/perimetro>5)

# Inspección visual
manzanas <- st_read("data/raw/GCABA/Manzanas/manzanas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(radios_cluster_16)

#radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
#    st_transform(crs=4326)

ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas, fill="grey85", color="grey40", size=.8)+
    geom_sf(data=parcelario_filter, fill="#8F00FF")+
#    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()

st_write(parcelario_filter, "data/processed/GCABA/parcelas_potenciales/parcelas_pontenciales_mas_de_5000_cluster_16.shp", delete_dsn = TRUE)

