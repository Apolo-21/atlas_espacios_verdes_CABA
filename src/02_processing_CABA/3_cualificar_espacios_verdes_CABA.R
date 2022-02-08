library(tidyverse)
library(sf)
library(leaflet)


##############################################################################################
# Ponderación de la superficie de espacios verdes de acuerdo a su condición de accesibilidad #
##############################################################################################

# Los espacios enrejados de la ciudad abren sus puertas entre 08:00 hs y 20:00 hs.
# Como consecuencia, solo es posible acceder a los mismos entre dichas horas.
# En base a esta situación, optamos por subponderar la superficie de los mismos 
# con el fin de que nuestro indice de accesebilidad a EV de cuenta de esta restricción
# temporal a la que se ven sujetos los vecinos de la CABA.

# Abrimos nuestro dataset con los espacios verdes (EV) de la Ciudad de Buenos Aires.
verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

#A partir del dataset, anterior creamos dos datasets: 
# - Espacios verdes abiertos.
abiertos <- verdes %>% 
    filter(cierre!="Cerrable")

# - Espacios verdes cerrados (EVC).
cerrados <- verdes %>% 
    filter(cierre=="Cerrable")


# ¿Cómo subponderamos la superficie de los espacios verdes cerrables (EVC)?

# 1º Extraemos las geometrías de nuestro dataset.
cerrados_sfc <- st_geometry(cerrados)

# 2º Para subponderar nuestras geometrías necesitamos un punto desde donde escalarlas.
# Para ello, obtenemos los centroides.
cerrados_sfc_centroides <- st_centroid(cerrados_sfc)

# 3º Luego, calculamos la superficie de cada polígono desde su centroide.
sfc_desde_centroide <- (cerrados_sfc - cerrados_sfc_centroides)

# 4* Establecemos un ponderador de superficie por el cual vamos a penalizar a los
# EVC por su limitante en la accesibidad.
# En este caso, vamos a definir una penalización del 33% de la superficie. Este 
# número se desprende de calcular las horas en los que los parques y plazas enrejadas
# se encuentran abiertas durante el día. En este marco, si se sostiene que la mayor 
# intensidad de uso de un espacio público ocurre entre 6 hs y 24 hs, se puede pensar
# que un parque cercado resulta inaccesible durante un tercio de su uso potencial.
# Por este motivo, de este razonamiento, se desprende que la falta de accesibilidad
# a los mismos puede ser penalizada a un 33% la superficie del espacio cerrable.
ponderador <- 0.67

# 5º multiplicamos nuestra superficie por el ponderador y le sumamos las coordenadas del centroide.
cerrados_scala <- sfc_desde_centroide*ponderador + cerrados_sfc_centroides

# 6º Remplazamos nuestras geometrías viejas por nuestra nueva superficie ponderada.
cerrados <- cerrados %>% 
    st_set_geometry(cerrados_scala)

# Asignamos el sistema de coordenadas orginal para poder unir nuestros datasets.
cerrados <- cerrados %>%
    st_set_crs(4326) 

# Visualización superficie ponderada de espacios verdes cerrables.
leaflet(cerrados) %>% 
    addPolygons() %>% 
    addTiles()

# Volvemos a juntar nuestros datasets de espacios abiertos y cerrados en uno que contenga ambos.
verdes_cualificados <- rbind(abiertos, cerrados)

# Visualización
leaflet(verdes_cualificados) %>% 
    addPolygons() %>% 
    addTiles()

# Guardamos
st_write(verdes_cualificados, "data/processed/GCABA/EV/espacios-verdes-ponderados.shp", delete_dsn = TRUE)
