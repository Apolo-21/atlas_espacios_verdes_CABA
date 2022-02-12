library(tidyverse)
library(sf)

########################################################
# Descarga de los barrios de la Ciudad de Buenos Aires #
########################################################

# Descargamos las geometrías de los barrios de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos "BA Data" (https://data.buenosaires.gob.ar/)

barrios <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson") %>% 
    janitor::clean_names() %>% 
    mutate(comuna = round(as.numeric(comuna)),
           comuna = as.factor(comuna),
           perimetro = round(as.numeric(perimetro), 2),
           area = round(as.numeric(area), 2))


# Guardamos
st_write(barrios, "data/raw/GCABA/Barrios/barrios.geojson", delete_dsn = TRUE)
