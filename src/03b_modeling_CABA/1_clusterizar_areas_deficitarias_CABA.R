library(sf)
library(tidyverse)
library(ggplot2)
library(igraph)

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp")

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>%
    st_intersection(CABA_limite)

EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE)

accesibilidad <- st_read("data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA.shp") %>% 
    st_transform(crs = "+proj=laea +lat_0=-40 +lon_0=-60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

# creamos un umbral de proximidad para agrupar dar un margen de tolerancia y luego clusterizar los buffers
umbral_de_proximidad <- 100

accesibilidad_deficitaria <- accesibilidad %>% 
    filter(situacn=="sin_acceso")

buffers <- st_buffer(accesibilidad_deficitaria, umbral_de_proximidad) 

solapamientos <- st_intersects(buffers, buffers)

# armamos el grafo a partir de la matriz de adyacencia generada por st_intersects()
grafo <- graph_from_adj_list(solapamientos)

# le asignamos a cada buffer el ID de grupo/cluster al que pertenece 

buffers <- buffers %>% 
    mutate(cluster_id = components(grafo)$membership) %>% 
    group_by(cluster_id) %>% 
    summarise()

# Le asignamos a cada espacio verde el grupo de proximidad al que pertenece

accesibilidad_deficitaria <- accesibilidad_deficitaria %>% 
    st_join(buffers) 

#Inspección visual

radios_CABA <- st_transform(radios_CABA, crs=4326)
accesibilidad_deficitaria <- st_transform(accesibilidad_deficitaria, crs=4326)
data <- st_transform(data, crs=4326)
EV <- st_transform(EV, crs=4326)
comunas <- st_transform(comunas, crs=4326)
CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326)


ggplot()+
    geom_sf(data=CABA_limite, color="black", size=1, fill=NA)+
    geom_sf(data=radios_CABA, fill="gray95", color="grey70")+
    geom_sf(data=accesibilidad_deficitaria, fill="#ffcd00", show.legend = FALSE) +
    geom_sf(data=accesibilidad_deficitaria %>% 
                dplyr::filter(cluster_id %in% c(12, 16)), fill="#8F00FF") +
    geom_sf(data=EV, fill="gray70", color="grey60")+
    geom_sf(data=comunas, fill=NA, size=.2, color="black", alpha=.3)+
    theme_void()


#guardamos
st_write(accesibilidad_deficitaria, "data/processed/accesibilidad/radios_con_accesibilidad_clusterizados.shp", delete_dsn = TRUE)

