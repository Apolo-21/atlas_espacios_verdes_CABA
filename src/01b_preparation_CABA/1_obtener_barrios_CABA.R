library(tidyverse)

######################################################
# Descargar los barrios de la Ciudad de Buenos Aires #
######################################################

# Descargamos las geometrías de los barrios de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos "BA Data" (https://data.buenosaires.gob.ar/)

barrios <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson") %>% 
    mutate(COMUNA=as.numeric(COMUNA),
           COMUNA=round(COMUNA,0),
           COMUNA=as.factor(COMUNA),
           PERIMETRO=as.numeric(PERIMETRO),
           PERIMETRO=round(PERIMETRO,2),
           AREA=as.numeric(AREA),
           AREA=round(AREA,2)) 

# Guardamos
st_write(barrios, "data/raw/GCABA/Barrios/barrios.shp", delete_dsn = TRUE)
