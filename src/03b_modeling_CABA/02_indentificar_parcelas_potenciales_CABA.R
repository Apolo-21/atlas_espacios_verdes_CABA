library(sf)
library(tidyverse)
library(ggplot2)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica

accesibilidad <- st_read("data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA.shp") %>% 
    filter(situacn=="sin_acceso")

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

# Identificaremos parcelas que cumplan con nuestros criterios (tamaños y relación area/perímetro) 
# como posibles localizaciones de nuevos EEVV dentro de nuestra área deficitaria

parcelario <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/parcelas/parcelas.geojson") %>% 
    st_transform(crs=4326) %>% 
    st_intersection(accesibilidad)

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
ggplot()+
    geom_sf(data=radios_CABA, fill=NA, fill="grey96", color="grey66")+
    geom_sf(data=accesibilidad, fill="grey") +
    geom_sf(data=parcelario_filter, fill="red")+
    geom_sf(data=EV, fill="#69b166", size=.1)+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.1)+
    theme_void()

st_write(parcelario_filter, "data/processed/GCABA/parcelas_potenciales/parcelas_pontenciales.shp", delete_dsn = TRUE)


