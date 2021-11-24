library(tidyverse)
library(sf)
library(ggplot2)
library(sp)
library(tbart)
library(osmdata)
library(GISTools)
#install.packages("shp2graph")
library(shp2graph)
  
#sf::sf_use_s2(FALSE) #apagamos la geometría esférica

# cargamos los radios censales, los espacios verdes y las calles de CABA
  
limite_CABA <- st_read("data/raw/OSM/limite_CABA.shp") %>% 
  st_transform(crs=4326)
 
calles <- st_read("data/raw/osm/callejero.shp") %>% 
  st_transform(crs=4326)
  
radios <- st_read("data/raw/INDEC/radios_CABA.shp") %>% 
  st_transform(crs=4326)
  
EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
  st_transform(crs=4326)
  
radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
  st_transform(crs=4326)
  
estacionamientos <- st_read("data/raw/osm/parking_CABA.shp") %>% 
  st_transform(crs=4326) %>% 
  dplyr:: select(osm_id)
  
  
# inspeccion visual
ggplot()+
  geom_sf(data=radios, fill="gray90")+
  geom_sf(data=calles, color="gray30")+
  geom_sf(data=EV, fill="gray70")+
  geom_sf(data=radios_cluster_16, fill="red")+
  theme_minimal()
  
  
## FALTA HACER UN RECORTE DE LAS CALLES
#####
  
buffer_para_recorte <- radios_cluster_16 %>%
  st_union() %>% 
  st_as_sf(crs=4326) %>% 
  st_buffer(10)
  
# recortes

estacionamientos <- estacionamientos %>% st_intersection (buffer_para_recorte)
calles <- calles %>% st_intersection(buffer_para_recorte)

ggplot()+
  geom_sf(data=radios_cluster_16, fill="grey80")+
  geom_sf(data=estacionamientos, color="red", size=3)+
  geom_sf(data=calles)+
  theme_void()

####

# Transformamos a SP 

radios_centroides_sp <- as_Spatial (radios_cluster_16, cast=TRUE)
calles_sp <- as_Spatial(calles, cast=TRUE)
estacionamientos_sp <- as_Spatial(estacionamientos, cast = TRUE)


# Extract the coordinates Existing facilities
oferta_potencial <- coordinates(estacionamientos_sp)
oferta_potencial <- cbind(oferta_potencial[,1]+0.008,oferta_potencial[,2]+0.008)

# Demand/Facilities (731 SA centroids)
demanda <- coordinates(radios_centroides_sp)
demanda <-cbind(demanda[,1]+0.008,demanda[,2]+0.008)

# Map the points to the nearest nodes on the network Takes roughly 2 mins
oferta_grilla <- points2network (ntdata = calles_sp, pointsxy = oferta_potencial, 
                           ELComputed = T)
# Takes roughly 5 mins
demanda_grilla <- points2network (ntdata = calles_sp, pointsxy = demanda, 
                         ELComputed = T)

###################
# Existing Facilities (Garda stations). IDs of corresponding nodes for data
# points
CoorespondIDs = oferta_grilla[[3]]
# Generate an 'igraph' object by weighting this graph with edge length
ig <- nel2igraph(oferta_grilla[[1]], oferta_grilla[[2]], weight = oferta_grilla[[8]])
gstation <- as.numeric(CoorespondIDs)
# Demand (SAs)
CoorespondIDs2 = demanda_grilla[[3]]
ig2 <- nel2igraph(demanda_grilla[[1]], demanda_grilla[[2]], weight = demanda_grilla[[8]])
SA <- as.numeric(CoorespondIDs2)

duplicated(CoorespondIDs2)

A <- as.numeric(CoorespondIDs2)
while (CoorespondIDs2(A) > 0) {
  i <- CoorespondIDs2(A)
  A[i] <- A[i] + 1
}
SA <- A

#################


# Euclidean distance matrix Between demand and existing Garda stations
EucDist1 <- euc.dists(radios_centroides_sp, estacionamientos_sp)
# Between demand and candidate facilities
EucDist2 <- euc.dists(radios_centroides_sp, radios_centroides_sp)

# Shortest-path distance matrix between demand and existing facilities
spm1 <- shortest.paths(ig, v = c(SA), to = c(oferta_potencial), weight = get.edge.attribute(ig, name = "weight"))


# Existing locations
model_1 <- allocations(radios_centroides_sp, estacionamientos_sp, p = 5)
# Create the spider diagram linking existing Garda stations to SA centroids
# (demand)
star.model_1 <- star.diagram(radios_centroides_sp, estacionamientos_sp, alloc = model_1$allocation)
# Optimal locations
model_2 <- allocations(radios_centroides_sp, radios_centroides_sp, p = 5)
# Create the spider diagram linking optimal locations to SA centroids
# (demand)
star.model_2 <- star.diagram(radios_centroides_sp, radios_centroides_sp, alloc = model_2$allocation)


# Calculate the total distance travelled between demand and facility given
# the present spatial distribution of Garda stations
sum(model_1$allocdist)
# Calculate the total distance travelled between demand and facility when
# Garda stations are optimally located
sum(model_2$allocdist)

# Returns the index number of optimally chosen facilities
optimal_loc <- unique(model_2$allocation)
optimal_loc <- radios_centroides_sp[optimal_loc, ]



#OPTIMO
plot(radios_cluster_16$geometry, bg=(alpha=.1), add = F)
plot(star.model_2, col="blue", add = T)
plot(optimal_loc, col = "black", cex = 1.5, lwd = 4, add = T)
title(main = "Puntos optimos", font.main = 6)


#INCOPORANDO INFRAE
plot(radios_cluster_16$geometry, bg=(alpha=.1), add = F)
plot(star.model_1, col="blue", add = T)
plot(estacionamientos_sp, col = "red", pch = 20, lwd = 4, add = T)
title(main = "Oferta existente", font.main = 6)



#DIFERENCIA
plot(radios_cluster_16$geometry, bg=(alpha=.1), add = F)
plot(optimal_loc, col = "red", cex = 1.5, lwd = 4, add = T)
plot(estacionamientos_sp, col = "royalblue3", pch = 20, lwd = 1, add = T)
title(main = "óptimo vs oferta", 
      font.main = 6)
legend(687000, 691328, legend = c("Existing locations", "Optimal locations"), 
       bty = "n", cex = 0.9, col = c("royalblue3", "orange"), pch = c(20, 3))



