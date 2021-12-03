library(tidyverse)
library(sf)
library(ggplot2)
library(sp)
library(tbart)

sf::sf_use_s2(TRUE)


################################################################################
# Parking + lotes vacantes y depositos de Properati - Cluster 16
################################################################################

# En este script vamos a averiguar si los lotes vacantes y depósitos relevados por Properati
# sumados a los estacionamientos son capaces de cubirir el área deficitaria de una manera mas eficiente

# Utilizaremos la misma metodología de los scripts anteriores

# cargamos todos los datasets que vamos a necesitar, y nos aseguramos de que su proyección sea la misma

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% #demanda insatisfecha
  st_transform(crs=proj)%>% 
    st_difference()

radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% #demanda insatisfecha
    st_transform(crs=proj)%>% 
    st_difference()
  
lotes_vacantes <- st_read("data/processed/GCABA/parcelas_potenciales/parcelas_vacantes_cluster_16.shp") %>% #lotes y depositos vacantes
  st_transform(crs=proj) %>% 
    dplyr:: filter(PARKING==1 | VACANTE==1) %>% 
    st_difference()


manzanas_cluster <- st_read("data/processed/GCABA/manzanas_con_parcelas_potenciales/manzanas_potenciales_cluster_16.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=proj)

distancia_a_centroides <- read.csv("data/processed/model/distancia_promedio_centroide_cluster.csv", header = FALSE) %>% 
    as.numeric() #distancia maxima promedio de las isocronas calculadas en el script 6a
#_______________________________________________________________________________

# COBERTURA OPTIMA

# Convertimos a SP

lotes_vacantes_sp <- as_Spatial(st_centroid(lotes_vacantes, cast = TRUE))
manzanas_cluster_sp <- as_Spatial (st_centroid(manzanas_cluster), cast=TRUE)
radios_sp <- as_Spatial (st_centroid(radios_cluster_16), cast=TRUE)

p=0 # parctimos de 0 centroides
repeat{
    
    p=p+1
    
    modelo_teorico <- allocations(radios_sp, manzanas_cluster_sp, p = p)
    
    if (max(modelo_teorico$allocdist) <= distancia_a_centroides) { 
        break
    }
}

# Creamos el diagrama para ver la cobertura
star.model_teorico <- star.diagram(radios_sp, manzanas_cluster_sp, alloc = modelo_teorico$allocation)

# Creamos un SpatialPointsDataframe de los punto óptimos obtenidos del 2do modelo
optimal_loc <- unique(modelo_teorico$allocation)
optimal_loc <- manzanas_cluster_sp[optimal_loc, ]

# lo pasamos a sf, lo necesitaremos luego
optimal_loc_points <- st_as_sf(optimal_loc, crs=4326)

# Modelo teórico óptimo
plot(radios_cluster_16$geometry, col="white", lwd=.5, lty=2, add = F) +
    plot(manzanas_cluster$geometry, col="grey85", lwd=.8, add=T) +
    plot(star.model_teorico, border =3, col="grey30", lty=2, lwd=2, add = T) +
    plot(optimal_loc, col = "black", lwd = 3, pch=13, cex=3, add = T)
#_______________________________________________________________________________

# COBERTURA REAL

### REPETIMOS PERO CON LOS DATOS DE DEPOSITOS Y TERRENOS VACANTES

p=0 # partimos de 0 centroides
repeat{
  
  p=p+1
  
  modelo_real <- allocations(radios_sp, lotes_vacantes_sp, p = p)
  
  if (max(modelo_real$allocdist) <= distancia_a_centroides) { 
    break
  }
}


hist(modelo_real$allocdist, xlim = c(0,1000), ylim = c(0,40), ylab="Frecuencia", xlab="Distancia euclidiana (m)")

# Creamos el diagrama para ver la cobertura
star.modelo_real <- star.diagram(radios_sp, lotes_vacantes_sp, alloc = modelo_real$allocation)

mean(modelo_real$allocdist)
max(modelo_real$allocdist) 

real_loc <- unique(modelo_real$allocation)
real_loc <- lotes_vacantes_sp[real_loc, ]

# Modelo real óptimo
plot(radios_cluster_16$geometry, col="white", lwd=.5, lty=2, add = F) +
    plot(manzanas_cluster$geometry, col="grey85", lwd=.8, add=T) +
    plot(lotes_vacantes_sp, col="#8F00FF", add = T) +
    plot(star.modelo_real, border =3, col="grey30", lty=2, lwd=2, add = T) +
    plot(optimal_loc, col = "black", lwd = 3, pch=13, cex=3, add = T)


#_______________________________________________________________________________

# Mapeo con ggplot

# Nos quedamos con los puntos optimos
real_loc_points <- real_loc %>%
    st_as_sf(crs=4326) %>% 
    rownames_to_column(var="allocation") %>% #convetimos en index en columna
    select(allocation) %>% 
    mutate(allocation=as.integer(allocation))

modelo_real_sf <- modelo_real %>% 
    as_data_frame() %>% 
    select(id, allocation) %>% 
    left_join(st_centroid(radios_cluster_16, by="id")) %>% 
    rename(geometry_radios=geometry) %>% 
    inner_join(real_loc_points %>% as.data.frame(), by="allocation") %>% 
    rename(geometry_puntos=geometry)

#vamos a separar las geometrías, en longitud y latitud para crear los segmentos

modelo_real_sf <- modelo_real_sf %>% 
    cbind(st_coordinates(modelo_real_sf$geometry_radios)) %>% 
    rename(lon_radios=X,
           lat_radios=Y) %>% 
    cbind(st_coordinates(modelo_real_sf$geometry_puntos)) %>% 
    rename(lon_puntos=X,
           lat_puntos=Y)

# ahora si estamos en condiciones de mapear:

ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas_cluster, fill="grey85", color="grey40", size=.8)+
    geom_sf(data=lotes_vacantes, fill="#8F00FF")+
    geom_segment(data = modelo_real_sf, aes(x = lon_radios, y = lat_radios, xend = lon_puntos, yend = lat_puntos), linetype="dashed", size=1)+
    geom_sf(data =  optimal_loc_points, size=5) +
    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()
