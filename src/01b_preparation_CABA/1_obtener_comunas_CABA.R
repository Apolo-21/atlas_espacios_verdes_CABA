library(tidyverse)
library(sf)

########################################################
# Descarga de las Comunas de la Ciudad de Buenos Aires #
########################################################

# Descargamos las geometrías de las Comunas de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos "BA Data" (https://data.buenosaires.gob.ar/)

comunas <- read_sf("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>% 
    janitor::clean_names() %>% 
    transmute(comuna = round(as.numeric(comunas)),
              comuna = as.factor(comuna),
              barrios = barrios,
              perimetro = round(as.numeric(perimetro), 2),
              area = round(as.numeric(area), 2))

# Guardamos
st_write(comunas, "data/raw/GCABA/Comunas/comunas.geojson", delete_dsn = TRUE)
