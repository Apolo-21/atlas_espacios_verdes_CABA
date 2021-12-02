library(sf)
library(tidyverse)

################################################################################
#Descargar los barrios de la Ciudad
################################################################################

# Descargamos las geometrías de los barrios de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos BA Data https://data.buenosaires.gob.ar/

barrios <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson") %>% 
    select(-wkt, -objeto) %>% 
    mutate(comuna=as.numeric(comuna),
           comuna=round(comuna,0),
           comuna=as.factor(comuna),
           perimetro=as.numeric(perimetro),
           perimetro=round(perimetro,2),
           area=as.numeric(area),
           area=round(area,2)) %>% 
    st_transform(barrios, crs = 4326) #sistema de coordenadas

# Guardamos
st_write(barrios, "data/raw/GCABA/Barrios/barrios.shp", delete_dsn = TRUE)
