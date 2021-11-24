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

estacionamiento_relevado <- st_read("data/raw/osm/estacionamientos_relevados.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=proj) %>% 
    dplyr:: select(osm_id)

estacionamientos_relevados <- st_read ("data/processed/GCABA/parcelas_potenciales/estacionamientos_por_parcela_cluster_16.shp") %>%   #infraestuctura vacante, oferta potencial
    st_transform(crs=proj)
  

buffer_para_recorte <- radios_cluster_16 %>%
    st_union() %>% 
    st_as_sf(crs=proj) %>% 
    st_buffer(10)

# recortamos las calles
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


p=0
repeat{
    p=p+1
    modelo <- allocations(radios_centroides_sp, estacionamientos_sp, p = p)
    
    if ((modelo$allocdist)<500){
        break
    }
}



test <- prueba (demanda=radios_centroides_sp, oferta=estacionamientos_sp, p=1)


prueba <- function(demanda, oferta, p=1) {
    
    p=p
    
    modelo <- allocations(demanda, oferta, p)
    
    while ((modelo$allocdist)>1000) {
        p = p+1}
}


test <- prueba (demanda=radios_centroides_sp, oferta=estacionamientos_sp, p=1)




model_1 <- allocations(radios_centroides_sp, estacionamientos_sp, p = 3)

# Creamos el diagrama entre oferta y demanda
# Demanda
star.model_1 <- star.diagram(radios_centroides_sp, estacionamientos_sp, alloc = model_1$allocation)
# Optimal locations





model_2 <- allocations(radios_centroides_sp, radios_centroides_sp, p = 3)
# Creamos el diagrama para los puntos óptimos
# Demanda
star.model_2 <- star.diagram(radios_centroides_sp, radios_centroides_sp, alloc = model_2$allocation)


#UNIDADADES!!! kms?? 

# Distancia total entre demanda y oferta del modelo 1 
sum(model_1$allocdist) #sumatoria de distancias (real) = 135914 m
mean(model_1$allocdist) #promedio 457 m
max(model_1$allocdist) #distancia máxima 1560 m

hist(model_1$allocdist)

# Distancia total óptima
sum(model_2$allocdist) #sumatoria de distancias (optimo) = 138152 m
mean(model_2$allocdist) #promedio 465 m
max(model_2$allocdist) #distancia máxima 1712 m

hist(model_2$allocdist)


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

