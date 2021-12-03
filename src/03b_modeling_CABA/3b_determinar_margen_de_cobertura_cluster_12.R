library(tidyverse)
library(ggplot2)
library(sf)

################################################################################
# Unificar las manchas deficitarias armando un margen de cobertura
################################################################################

# Repetimos el paso previo que realizamos con el cluster 16, para mantenernos dentro del margen de seguridad

# Cargamos las bases de datos

radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% 
    rename(id=PAIS0210_I)
    
accesibilidad_deficitaria <- st_read("data/processed/accesibilidad/radios_con_accesibilidad_clusterizados.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

#_______________________________________________________________________________

# CLUSTER 12
# Seleccionamos el cluster 12 y le damos un margen de tolerancia

umbral_de_tolerancia <- 75

cluster_12_buffer <- accesibilidad_deficitaria %>% 
    filter(cluster_id==12) %>% 
    st_buffer(umbral_de_tolerancia)

# centroide de los radios
radios_centroid <- radios %>% st_centroid(radios)

# unimos el buffer al centroide y nos quedamos sólo con el id y el cluster_id
radios_cluster_12 <- st_intersection (radios_centroid, cluster_12_buffer) 
radios_cluster_12 <- radios_cluster_12 %>% 
    as.data.frame() %>% 
    dplyr:: select(id, cluster_id)

#ahora sí: 
radios_cluster_12 <- radios %>% 
    filter(id %in% c(radios_cluster_12$id)) %>% 
    left_join(radios_cluster_12, by="id") %>% 
    dplyr::select(id, cluster_id, TOT_POB)

#_______________________________________________________________________________
# inspeccion visual
ggplot()+
    geom_sf(data=radios_cluster_12, color="grey40")+
    geom_sf(data=accesibilidad_deficitaria %>% 
                filter(cluster_id==12), fill="grey20", alpha=.5)+
    theme_void()


# guardamos los radios deficitarios finales para proceder al modelo
st_write(radios_cluster_12, "data/processed/accesibilidad/radios_cluster_12.shp", delete_dsn = TRUE)







