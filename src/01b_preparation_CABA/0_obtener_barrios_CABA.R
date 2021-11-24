library(sf)
library(janitor)
library(tidyverse)


# Descargamos las geometrías de los barrios de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos BA Data https://data.buenosaires.gob.ar/

barrios <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson") %>% 
    clean_names() %>% 
    select(-wkt, -objeto) %>% 
    mutate(comuna=as.numeric(comuna),
           comuna=round(comuna,0),
           comuna=as.factor(comuna),
           perimetro=as.numeric(perimetro),
           perimetro=round(perimetro,2),
           area=as.numeric(area),
           area=round(area,2))

# Agregamos el sistema de coordenadas estándar
barrios <- st_transform(barrios, crs = 4326)

# Guardamos
st_write(barrios, "data/raw/GCABA/Barrios/barrios.shp")
