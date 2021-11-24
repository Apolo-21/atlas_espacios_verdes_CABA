library(tidyverse)
library(sf)
library(ggplot2)
library(sp)
library(tbart)
library(osmdata)

sf::sf_use_s2(TRUE)

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# cargamos todos los datasets que vamos a necesitar, y nos aseguramos de que su proyección sea la misma
  
CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
  st_difference() %>% 
  st_transform(crs=proj)

radios <- st_read("data/raw/INDEC/radios_CABA.shp") %>% 
  st_transform(crs=proj)
  
EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
  st_transform(crs=proj)
  
radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% #demanda insatisfecha
  st_transform(crs=proj)
  
manzanas_cluster <- st_read("data/processed/GCABA/manzanas_con_parcelas_potenciales/manzanas_potenciales_cluster_16.shp") %>% #infraestuctura vacante, oferta potencial
  st_transform(crs=proj)

manzanas_potenciales <- manzanas_cluster %>% 
  dplyr:: filter(PARKING==1)

  
# inspeccion visual
ggplot()+
  geom_sf(data=radios, fill="gray90")+
  geom_sf(data=calles, color="gray30")+
  geom_sf(data=EV, fill="gray70")+
  geom_sf(data=manzanas_cluster, fill="red", color="gray40", alpha=.1)+
  geom_sf(data=manzanas_potenciales, color="red", fill="red")+
  theme_void()


# Nos basamos en la metodología de Billy Archbold, disponible en https://rstudio-pubs-static.s3.amazonaws.com/205171_4be5af4f7dea4bbc8dca5de2b0670daa.html#data_preparation
# Para utilizar la librería tbart (necesitamos transformar nuestros sf a spatial)

manzanas_cluster_sp <- as_Spatial (st_centroid(manzanas_cluster), cast=TRUE)
manzanas_potenciales_sp <- as_Spatial (st_centroid(manzanas_potenciales), cast=TRUE)


# T-bart para resolver el problema de la p-mediana

### ¿cuántos centroides necesitamos?

# Proponemos una cantidad teórica de puntos en base al área promedio de isocronas

# a ) cálculo promedio por área:

isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
  st_transform(crs =proj) %>% 
  mutate(area=st_area(.)) 

isocrona_cobertura_promedio <- as.numeric(mean(isocronas$area)) # promedio del área que cubren las isocronas de CABA

#centroides mínimos por área:
as.numeric(st_area(buffer_para_recorte))/isocrona_cobertura_promedio 

# si fuera por área necesitaríamos 2 centroides para cubrir la demanda insatisfecha, 
# sin embargo este cálculo no tiene en cuenta la foma de la mancha a cubrir, 
# como tiene un aspecto más lineal, las distancias máximas van a ser muy grandes
# vamos por otra opción más ajustada:

# b ) cálculo por distancia máxima a centroides (euclindiana):

# creamos una funcion que arroje las distancias máximas de los centroides a los vértices

furthest <- function(sf_object) {
  # tmpfun find the furthest point from the centroid of one unique polygon
  tmpfun <- function(x) {
    centroides <- st_centroid(x)
    vertices <-  st_coordinates(x)[,1:2]
    vertices <-  st_as_sf(as.data.frame(vertices), coords = c("X", "Y"))
    furthest <- max (st_distance(centroides, vertices)) #buscamos las distancias máximas desde los centroides a los vertices
    return(furthest)
  }
  
  # aplicamos la funciona todas las isocronas
  return(lapply(st_geometry(sf_object), tmpfun))
}

# corremos la función para nuestras isocronas
distancias_maximas_promedio <- furthest(isocronas)

# nos quedamos con las distancias máximas promedio
distancia_a_centroides <- as.numeric(mean(sapply(distancias_maximas_promedio, mean)))


# Teniendo una distancia promedio de los centroides a los bordes más lejanos, vamos a averiguar cuántos centroides necesitamos
# para nuestros radios censales sin accesibilidad

p=0 # parctimos de 0 centroides
repeat{
  
  # iteramos, agregando un centoide por vez, hasta alcanzar el criterio que le pasamos  
  # critrio: dist máx del modelo
  p=p+1
  
  modelo_teorico <- allocations(manzanas_cluster_sp, manzanas_cluster_sp, p = p)
  
  if (max(modelo_teorico$allocdist) <= distancia_a_centroides) { 
    break
  }
}

# Necesitamos 4 centroides para cubrir las distancias

hist(modelo_teorico$allocdist)

# Creamos el diagrama para ver la cobertura
star.model_teorico <- star.diagram(manzanas_cluster_sp, manzanas_cluster_sp, alloc = modelo_teorico$allocation)

mean(modelo_teorico$allocdist) #distancia euclidiana promedio 321 m
max(modelo_teorico$allocdist) # euclidiana distancia máxima 903 m

# Creamos un SpatialPointsDataframe de los punto óptimos obtenidos del 2do modelo
optimal_loc <- unique(modelo_teorico$allocation)
optimal_loc <- manzanas_cluster_sp[optimal_loc, ]

# Modelo teórico óptimo
plot(radios_cluster_16$geometry, col="grey90", bg=(alpha=.1), add = F)
plot(star.model_teorico, col="grey20", lty=2, add = T)
plot(optimal_loc, col = "darkred", lwd = 20, add = T)
title(main = "Puntos optimos", font.main = 6)



### REPETIMOS PERO CON LOS DATOS DE ESTACIONAMIENTOS RELEVADOS

p=0 # parctimos de 0 centroides
repeat{
  
  p=p+1
  
  modelo_real <- allocations(radios_centroides_sp, manzanas_potenciales_sp, p = p)
  
  if (max(modelo_real$allocdist) <= distancia_a_centroides) { 
    break
  }
}

# Necesitamos 7 centroides para cubrir la demanda insatisfecha dada la oferta existente

hist(modelo_real$allocdist)

# Creamos el diagrama para ver la cobertura
star.modelo_real <- star.diagram(radios_centroides_sp, manzanas_potenciales_sp, alloc = modelo_real$allocation)

mean(modelo_real$allocdist) #distancia euclidiana promedio 290 m
max(modelo_real$allocdist) # euclidiana distancia máxima 645 m

# Modelo real óptimo
plot(radios_cluster_16$geometry, col="grey90", bg=(alpha=.1), add = F)
plot(manzanas_potenciales, col="#8F00FF", add = T) 
plot(optimal_loc, col = "darkred", lwd = 5, add = T)
plot(star.modelo_real, col="grey20", lty=2, add = T)
title(main = "Cobertura potencial Vs. Cobertura ópitma", font.main = 20)



# ________________________________________________________________________

### CONVIERTO TODO A SF

radios_cluster_16_sf <- st_as_sf(radios_cluster_16, crs=4326)
estacionamientos_sf <- st_as_sf(estacionamientos_sp, crs=4326)
modelo1_sf <- star.model_1 %>% 
  st_as_sf(crs=proj) %>% 
  st_set_crs(proj)



ggplot()+
  geom_sf(data=radios_cluster_16_sf, fill="grey90")+
  geom_sf(data=modelo1_sf, aes(geometry=geometry), color="grey40", linetype="dashed")+
  geom_sf(data=estacionamientos_sf, aes(geometry=geometry), color="darkred") + 
  theme_void()

