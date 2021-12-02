library(tidyverse)
library(sf)
library(ggplot2)
library(sp)
library(tbart)

sf::sf_use_s2(TRUE)

proj <- "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# cargamos todos los datasets que vamos a necesitar, y nos aseguramos de que su proyección sea la misma
#_______________________________________________________________________________

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
  st_difference() %>% 
  st_transform(crs=proj)

radios <- st_read("data/raw/INDEC/radios_CABA.shp") %>% 
  st_transform(crs=proj)
  
EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
  st_transform(crs=proj)
  
radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% #demanda insatisfecha
  st_transform(crs=proj)
  
manzanas_cluster <- st_read("data/processed/GCABA/manzanas_con_parcelas_potenciales/manzanas_potenciales_cluster_12.shp") %>% #infraestuctura vacante, oferta potencial
    st_transform(crs=proj)

parcelas_cluster <- st_read("data/processed/GCABA/parcelas_potenciales/parcelas_potenciales_cluster_12.shp") %>% #infraestuctura vacante, oferta potencial
  st_transform(crs=proj)

parcelas_potenciales <- parcelas_cluster %>% 
  dplyr:: filter(PARKING==1)
#_______________________________________________________________________________


# inspeccion visual
ggplot()+
    geom_sf(data=radios, fill="gray95", color="grey70")+
    geom_sf(data=EV, fill="gray70", color="grey60")+
    geom_sf(data=radios_cluster_12, color=NA, fill="#8F00FF", alpha=.1)+
    geom_sf(data=parcelas_potenciales, color="#8F00FF")+
    theme_void()


# Nos basamos en la metodología de Billy Archbold, disponible en https://rstudio-pubs-static.s3.amazonaws.com/205171_4be5af4f7dea4bbc8dca5de2b0670daa.html#data_preparation
# Para utilizar la librería tbart (necesitamos transformar nuestros sf a spatial)

manzanas_cluster_sp <- as_Spatial (st_centroid(manzanas_cluster), cast=TRUE)
parcelas_potenciales_sp <- as_Spatial (st_centroid(parcelas_potenciales), cast=TRUE)
radios_sp <- as_Spatial (st_centroid(radios_cluster_12), cast=TRUE)


# T-bart para resolver el problema de la p-mediana

### ¿cuántos centroides necesitamos?

# Proponemos una cantidad teórica de puntos en base al área promedio de isocronas

# a ) cálculo promedio por área:

isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
  st_transform(crs =proj) %>% 
  mutate(area=st_area(.)) 

isocrona_cobertura_promedio <- as.numeric(mean(isocronas$area)) # promedio del área que cubren las isocronas de CABA

#centroides mínimos por área:
buffer_para_recorte <- radios_cluster_16 %>% #aprovechamos a crear un buffer que nos servirá para recortar la geometría 
    st_union() %>% 
    st_as_sf(crs=proj)

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
  
  modelo_teorico <- allocations(radios_sp, manzanas_cluster_sp, p = p)
  
  if (max(modelo_teorico$allocdist) <= distancia_a_centroides) { 
    break
  }
}

# Necesitamos 4 centroides para cubrir las distancias

hist(modelo_teorico$allocdist)

# Creamos el diagrama para ver la cobertura
star.model_teorico <- star.diagram(radios_sp, manzanas_cluster_sp, alloc = modelo_teorico$allocation)

mean(modelo_teorico$allocdist) #distancia euclidiana promedio 321 m
max(modelo_teorico$allocdist) # euclidiana distancia máxima 903 m

# Creamos un SpatialPointsDataframe de los punto óptimos obtenidos del 1er modelo
optimal_loc <- unique(modelo_teorico$allocation)
optimal_loc <- manzanas_cluster_sp[optimal_loc, ]

# Modelo teórico óptimo


plot(radios_cluster_12$geometry, col="white", lwd=.5, lty=2, add = F) +
    plot(manzanas_cluster$geometry, col="grey85", lwd=.8, add=T) +
    plot(star.model_teorico, border =3, col="grey30", lty=2, lwd=2, add = T) +
    plot(optimal_loc, col = "black", lwd = 3, pch=13, cex=3, add = T)

#_______________________________________________________________________________
### Visualizacion con ggplot
## vamos a convertir los objetos SP a SF
## dado que el star.diagram no tiene sistema de coodenadas, vamos a crear segmentos asociando los puntos de geometrías 
## es decir, segmentos desde los centroides de manzanas hasta el punto teórico óptimo asociado

# Nos quedamos con los puntos optimos
optimal_loc_points <- optimal_loc %>%
    st_as_sf(crs=4326) %>% 
    rownames_to_column(var="allocation") %>% #convetimos en index en columna
    select(allocation) %>% 
    mutate(allocation=as.integer(allocation))

# Ahora nos quedamos con los centroides de los radios asociados al punto optimo que pertenencen
modelo_teorico_sf <- modelo_teorico %>% 
    st_as_sf(crs=4326) %>% 
    as_data_frame() %>% 
    select(id, allocation) %>% 
    left_join(st_centroid(radios_cluster_12), by="id") %>% 
    rename(geometry_radios=geometry) %>% 
    inner_join(optimal_loc_points %>% as.data.frame(), by="allocation") %>% 
    rename(geometry_puntos=geometry)

#vamos a separar las geometrías, en longitud y latitud para crear los segmentos
modelo_teorico_sf <- modelo_teorico_sf %>%     
    cbind(st_coordinates(modelo_teorico_sf$geometry_radios)) %>% 
    rename(lon_radios=X,
           lat_radios=Y) %>% 
    cbind(st_coordinates(modelo_teorico_sf$geometry_puntos)) %>% 
    rename(lon_puntos=X,
           lat_puntos=Y)

radios_cluster_16 <- st_read("data/processed/accesibilidad/radios_cluster_16.shp") %>% 
    st_transform(proj)


# ahora si estamos en condiciones de mapear
ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40", fill=NA, linetype="dashed", size=.5)+
    geom_sf(data=manzanas_cluster, fill="grey85", color="grey40", size=.8)+
    geom_segment(data = modelo_teorico_sf, aes(x = lon_radios, y = lat_radios, xend = lon_puntos, yend = lat_puntos), linetype="dashed", size=1)+
    geom_sf(data =  optimal_loc_points, size=5) +
    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()

#_______________________________________________________________________________


### REPETIMOS PERO CON LOS DATOS DE ESTACIONAMIENTOS RELEVADOS

p=0 # parctimos de 0 centroides
repeat{
  
  p=p+1
  
  modelo_real <- allocations(radios_sp, parcelas_potenciales_sp, p = p)
  
  if (max(modelo_real$allocdist) <= distancia_a_centroides) { 
    break
  }
}

# Necesitamos 5 centroides para cubrir la demanda insatisfecha dada la oferta existente

hist(modelo_real$allocdist)

# Creamos el diagrama para ver la cobertura
star.modelo_real <- star.diagram(radios_sp, parcelas_potenciales_sp, alloc = modelo_real$allocation)

mean(modelo_real$allocdist) #distancia euclidiana promedio 290 m
max(modelo_real$allocdist) # euclidiana distancia máxima 645 m

real_loc <- unique(modelo_real$allocation)
real_loc <- parcelas_potenciales_sp[real_loc, ]

# Modelo real óptimo
plot(radios_cluster_12$geometry, col="white", lwd=.5, lty=2, add = F) +
    plot(manzanas_cluster$geometry, col="grey85", lwd=.8, add=T) +
    plot(parcelas_potenciales$geometry, col="#8F00FF", add = T) +
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
    st_as_sf(crs=4326) %>% 
    as_data_frame() %>% 
    select(id, allocation) %>% 
    left_join(st_centroid(radios_cluster_12, by="id")) %>% 
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
    geom_sf(data=parcelas_potenciales, fill="#8F00FF")+
    geom_segment(data = modelo_real_sf, aes(x = lon_radios, y = lat_radios, xend = lon_puntos, yend = lat_puntos), linetype="dashed", size=1)+
    geom_sf(data =  optimal_loc_points, size=5) +
    geom_sf(data = radios_cluster_12, fill=NA, linetype="dashed")+ # ubicamos el cluster vecino
    theme_void()

