library(tidyverse)
library(ggplot2)
library(sf)

radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs") %>% 
    rename(id=PAIS0210_I)
    
accesibilidad_deficitaria <- st_read("data/processed/accesibilidad/radios_con_accesibilidad_clusterizados.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")


# CLUSTER 16
# Seleccionamos el cluster 16 y le damos un margen de tolerancia

umbral_de_tolerancia <- 75

cluster_16_buffer <- accesibilidad_deficitaria %>% 
    filter(cluster_id==16) %>% 
    st_buffer(umbral_de_tolerancia)

# centroide de los radios
radios_centroid <- radios %>% st_centroid(radios)

# unimos el buffer al centroide y nos quedamos sólo con el id y el cluster_id
radios_cluster_16 <- st_intersection (radios_centroid, cluster_16_buffer) 
radios_cluster_16 <- radios_cluster_16 %>% 
    as.data.frame() %>% 
    select(id, cluster_id)

#ahora sí: 
radios_cluster_16 <- radios %>% 
    filter(id %in% c(radios_cluster_16$id)) %>% 
    left_join(radios_cluster_16, by="id") %>% 
    select(id, cluster_id, TOT_POB)

# inspeccion visual
ggplot()+
    geom_sf(data=radios_cluster_16, color="blue")+
    geom_sf(data=accesibilidad_deficitaria %>% 
                filter(cluster_id==16), fill="red", alpha=.5)+
    theme_void()


# guardamos los radios deficitarios finales para proceder al modelo
st_write(radios_cluster_16, "data/processed/accesibilidad/radios_cluster_16.shp")







