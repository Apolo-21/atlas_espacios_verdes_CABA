library(tidyverse)
library(ggplot2)
library(sf)

################################################################################
# Unificar las manchas deficitarias armando un margen de cobertura
################################################################################

# Para mantenernos dentro del margen de seguridad y unificar las manchas deficitarias
# vamos a agrandar levemente la mancha sin accesibilidad 

# Cargamos las bases de datos

radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% 
    rename(id=PAIS0210_I)
    
accesibilidad_deficitaria <- st_read("data/processed/accesibilidad/radios_con_accesibilidad_clusterizados.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

#_______________________________________________________________________________

# MARGEN CLUSTER 16
# Seleccionamos el cluster 16 y le damos un margen de tolerancia

umbral_de_tolerancia <- 75 #superior a media cuadra standard
# si el centroide del radio censal vecino cae dentro del umbral de tolerancia, lo incluimos

cluster_16_buffer <- accesibilidad_deficitaria %>% 
    filter(cluster_id==16) %>% 
    st_buffer(umbral_de_tolerancia)

# centroide de los radios
radios_centroid <- radios %>% st_centroid(radios)

# unimos el buffer al centroide y nos quedamos sólo con el id y el cluster_id
radios_cluster_16 <- st_intersection (radios_centroid, cluster_16_buffer) 
radios_cluster_16 <- radios_cluster_16 %>% 
    as.data.frame() %>% 
    dplyr:: select(id, cluster_id)

#ahora sí: 
radios_cluster_16 <- radios %>% 
    filter(id %in% c(radios_cluster_16$id)) %>% 
    left_join(radios_cluster_16, by="id") %>% 
    dplyr::select(id, cluster_id, TOT_POB)

#_______________________________________________________________________________
# inspeccion visual
ggplot()+
    geom_sf(data=radios_cluster_16, color="grey40")+
    geom_sf(data=accesibilidad_deficitaria %>% 
                filter(cluster_id==16), fill="grey20", alpha=.5)+
    theme_void()


# guardamos los radios deficitarios finales para proceder al modelo
st_write(radios_cluster_16, "data/processed/accesibilidad/radios_cluster_16.shp", delete_dsn = TRUE)







