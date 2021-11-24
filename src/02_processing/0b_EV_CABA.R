#Nos quedamos sólo con los EV de CABA

library(sf)
library(tidyverse)
#install.packages("ggplot")
library(ggplot)
#install.packages("skimr")
library(skimr)

barrios_CABA <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/barrios/barrios.geojson", 
                        stringsAsFactors = F) %>% 
    select(-WKT) #CHECKEAR!!

# Convertimos al sistema de proyección equiareal para una medición precisa de áreas y distancias
barrios_CABA <- barrios_CABA %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

#Cargamos las áreas verdes de Argentina
#Hacemos una union espacial para quedarnos sólo con las que pertenecen a CABA
areas_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp", 
                   stringsAsFactors = F) %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


EV_CABA <- st_intersection(areas_verdes, barrios_CABA)

st_write(areas_verdes, "data/processed/osm/areas_verdes_CABA.shp", delete_dsn = TRUE)


#visualizamos
ggplot()+
    geom_sf(data=barrios_CABA)+
    geom_sf(data=EV_CABA, fill="darkgreen")


#Hay 705 espacios verdes
skimr::skim (EV_CABA)




#A TODA LA PARTE DE VISUALZIACIÓN Y ANALISIS LA HARIA EN UN ARCHIVO APARTE












