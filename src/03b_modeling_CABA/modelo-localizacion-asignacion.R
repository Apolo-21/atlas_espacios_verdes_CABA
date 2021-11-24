library(tidyverse)
library(sf)
library(ggplot2)
library(sp)
library(tbart)
library(osmdata)
library(GISTools)
library(shp2graph)

sf::sf_use_s2(TRUE) #apagamos la geometría esférica

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# cargamos todos los datasets que vamos a necesitar, y nos aseguramos de que su proyección sea la misma
  
limite_CABA <- st_read("data/raw/OSM/limite_CABA.shp") %>% 
  st_transform(crs=4326)
 
calles <- st_read("data/raw/osm/callejero.shp") %>% #calles de OSM
  st_transform(crs=proj)
  
radios <- st_read("data/raw/INDEC/radios_CABA.shp") %>% 
  st_transform(crs=proj)
  
EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
  st_transform(crs=proj)
  
radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% #demanda insatisfecha
  st_transform(crs=proj)
  
estacionamientos <- st_read("data/raw/osm/parking_CABA.shp") %>% #infraestuctura vacante, oferta potencial
  st_transform(crs=proj) %>% 
  dplyr:: select(osm_id)
  
  
# inspeccion visual
ggplot()+
  geom_sf(data=radios, fill="gray90")+
  geom_sf(data=calles, color="gray30")+
  geom_sf(data=EV, fill="gray70")+
  geom_sf(data=radios_cluster_16, fill="red")+
  theme_minimal()
  
  
## Hacemos un recorte del área afectada
  
buffer_para_recorte <- radios_cluster_16 %>%
  st_union() %>% 
  st_as_sf(crs=proj) %>% 
  st_buffer(10)
  
estacionamientos <- estacionamientos %>% st_intersection (buffer_para_recorte)
calles <- calles %>% st_intersection(buffer_para_recorte)

ggplot()+
  geom_sf(data=radios_cluster_16, fill="grey80")+
  geom_sf(data=estacionamientos, color="red", size=3)+
  geom_sf(data=calles)+
  theme_void()


# Vamos a seguir la metodología de Billy Archbold, disponible en https://rstudio-pubs-static.s3.amazonaws.com/205171_4be5af4f7dea4bbc8dca5de2b0670daa.html#data_preparation
# Para utilizar la librería tbart (necesitamos transformar nuestros sf a spatial

radios_centroides_sp <- as_Spatial (st_centroid(radios_cluster_16), cast=TRUE) 
calles_sp <- as_Spatial(calles, cast=TRUE)
estacionamientos_sp <- as_Spatial(estacionamientos, cast = TRUE)

# Extrar coordenadas de la demanda (radios no cubiertos)
demanda <- coordinates(radios_centroides_sp)
demanda <-cbind(demanda[,1],demanda[,2])

# Extrar coordenadas de la oferta potencial
oferta_potencial <- coordinates(estacionamientos_sp)
oferta_potencial <- cbind(oferta_potencial[,1],oferta_potencial[,2])


# Mapeamos los puntos de la oferta y demanda potencial en nuestra grilla de calles 

oferta_grilla <- points2network (ntdata = calles_sp, pointsxy = oferta_potencial, 
                           ELComputed = T)
demanda_grilla <- points2network (ntdata = calles_sp, pointsxy = demanda, 
                         ELComputed = T)


###################
CoorespondIDs <- oferta_grilla[[3]] #Ids de la grilla

# Generamos grafos con el peso de la distancia
ig <- nel2igraph(oferta_grilla[[1]], oferta_grilla[[2]], weight = oferta_grilla[[8]])
parking <- as.numeric(CoorespondIDs)

# Demanda
CoorespondIDs3 = demanda_grilla[[3]]
ig2 <- nel2igraph(demanda_grilla[[1]], demanda_grilla[[2]], weight = demanda_grilla[[8]])
census <- as.numeric(CoorespondIDs3)


duplicated(CoorespondIDs3)

# eliminamos los nodos duplicados
B <- as.numeric(CoorespondIDs3)
while (anyDuplicated(B) > 0) {
  i <- anyDuplicated(B)
  B[i] <- B[i] + 1
}
census <- B


# T-bart para resolver el problema de la p-mediana

# Distancia euclidiana entre los radios y los potenciales nuevos parques
EucDist1 <- euc.dists(radios_centroides_sp, estacionamientos_sp)

# Dustabcua euclidiana entre radios, para obtener luego la ubicacion teórica óptima
EucDist2 <- euc.dists(radios_centroides_sp, radios_centroides_sp)

# Camino más corto entre la demanda y la oferta potencial
shortest_paths_1 <- shortest.paths(ig, v = c(census), to = c(oferta_potencial), weight = get.edge.attribute(ig, name = "weight"))


# ¿Cuántos centroides necesitamos?
# Proponemos una cantidad teórica de puntos en base al área promedio de isocronas

isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
  st_transform(crs =proj) %>% 
  mutate(area=st_area(.))

isocrona_cobertura_promedio <- as.numeric(mean(isocronas$area))

#centroides mínimos:
as.numeric(st_area(buffer_para_recorte))/isocrona_cobertura_promedio #vamos a probar con 2 centroides



### MODELO
model_1 <- allocations(radios_centroides_sp, estacionamientos_sp, p = 2)

# Creamos el diagrama entre oferta y demanda
# Demanda
star.model_1 <- star.diagram(radios_centroides_sp, estacionamientos_sp, alloc = model_1$allocation)
# Optimal locations

model_2 <- allocations(radios_centroides_sp, radios_centroides_sp, p = 2)
# Creamos el diagrama para los puntos óptimos
# Demanda
star.model_2 <- star.diagram(radios_centroides_sp, radios_centroides_sp, alloc = model_2$allocation)


#UNIDADADES!!! kms?? 

# Distancia total entre demanda y oferta del modelo 1 
sum(model_1$allocdist) #sumatoria de distancias (real) = 135914 m
mean(model_1$allocdist) #promedio 457 m
max(model_1$allocdist) #distancia máxima 1560 m

# Distancia total óptima
sum(model_2$allocdist) #sumatoria de distancias (optimo) = 138152 m
mean(model_2$allocdist) #promedio 465 m
max(model_2$allocdist) #distancia máxima 1712 m

# Creamos un SpatialPointsDataframe de los punto óptimos obtenidos del 2do modelo
optimal_loc <- unique(model_2$allocation)
optimal_loc <- radios_centroides_sp[optimal_loc, ]


#OPTIMO
plot(radios_cluster_16$geometry, col="grey90", bg=(alpha=.1), add = F)
plot(star.model_2, col="grey20", lty=2, add = T)
plot(optimal_loc, col = "darkred", lwd = 20, add = T)
title(main = "Puntos optimos", font.main = 6)


#INCOPORANDO INFRAESTRUCTURA POTENCIAL
plot(radios_cluster_16$geometry, col="grey90", bg=(alpha=.1), add = F)
plot(star.model_1, col="grey20", lty=2, add = T)
plot(estacionamientos_sp, col = "darkblue", lwd = 10, add = T)
title(main = "Oferta existente", font.main = 6)


#DIFERENCIA
plot(radios_cluster_16$geometry, col="grey90", bg=(alpha=.1), add = F)
plot(star.model_2, col="grey20", lty=2, add = T)
plot(optimal_loc, col = "red", cex = 1.5, lwd = 4, add = T)
plot(estacionamientos_sp, col = "darkblue", pch = 20, lwd = 1, add = T)
title(main = "óptimo vs oferta", 
      font.main = 6)



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

