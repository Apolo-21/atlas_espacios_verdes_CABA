library(sf)
library(igraph)
library(tidyverse)

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
ggplot()+
    geom_sf(data=radios_CABA, fill="grey96", color="grey66")+
    geom_sf(data=accesibilidad_deficitaria, aes(fill=as.character(cluster_id)), show.legend = FALSE) +
    geom_sf(data=EV, fill="grey50", size=.1)+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.1)+
    theme_void()

# Cluster ID
ggplot()+
    geom_sf(data=radios_CABA, fill="white", color="grey66")+
    geom_sf(data=accesibilidad_deficitaria, aes(fill=as.character(cluster_id)), show.legend = FALSE) +
    geom_sf_text(data=accesibilidad_deficitaria, aes(label=as.character(cluster_id)))+
    theme_void()


# guardamos
st_write(accesibilidad_deficitaria, "data/processed/accesibilidad/radios_con_accesibilidad_clusterizados.shp", delete_dsn = TRUE)











