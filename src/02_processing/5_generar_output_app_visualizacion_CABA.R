library(sf)
library(tidyverse)


# Umbral de corte para áreas de espacios verdes
# Consideramos un mínimo de media hectárea, o 5000 m2, 
# siguiendo los lineamientos de los Indicadores Europeos de Sustentabilidad
# (https://www.gdrc.org/uem/footprints/eci_final_report.pdf). 
# que miden el porcentaje de habitantes que reside a menos de 300 metros lineales de un espacio abierto y público 
# de al menos media hectárea.

umbral_area_m2 <- 5000

areas_verdes <- st_read("data/processed/osm/areas_verdes_urbanas_argentina.shp") %>% 
    group_by(cluster_id) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2) %>% 
    st_transform(crs = 4326)

radios <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    filter(tiporad == "U") %>% 
    mutate(id = as.integer(id)) %>% 
    st_transform(crs = 4326)

metricas <- read_csv("data/processed/metricas/accesibilidad_espacios_verdes_aglomerados.csv") %>% 
    filter(!is.na(decil_NSE))

accesibilidad <- read_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando.csv")

# Obtenemos aglomerados y las coordenadas de sus centroides
aglomerados <- radios %>% 
    # Maldito error de tipeo en la data de origen
    mutate(eph_aglome = ifelse(eph_aglome == "San Nicolas - Villa Constitiución",
                               "San Nicolas - Villa Constitución",
                               eph_aglome)) %>% 
    group_by(eph_aglome) %>% 
    summarise %>% 
    mutate(lon = st_coordinates(st_centroid(.))[,1],
           lat = st_coordinates(st_centroid(.))[,2])

# Incluimos estadísticas agregadas de accesibilidad

aglomerados <- aglomerados %>% 
    left_join(metricas %>%
                  group_by(eph_aglome) %>% 
                  summarise(tasa_acceso = sum(poblacion * tasa_acceso) / sum(poblacion),
                            m2_accesibles_per_capita = sum(poblacion * m2_accesibles_per_capita) / sum(poblacion))) %>% 
    mutate(tasa_acceso = round(tasa_acceso, 3),
           m2_accesibles_per_capita = round(m2_accesibles_per_capita))

    
# radios con coding accesible/no accesible, y con al superficie de las áreas verdes recortada, 
# para el basemap hosteado en MapBox

radios_para_basemap <- radios %>% 
    left_join(accesibilidad) %>% 
    mutate(con_acceso = total_ha > 0) %>% 
    group_by(con_acceso) %>% 
    summarise() %>% 
    st_difference(st_union(areas_verdes))

# A guardar

st_write(aglomerados, "src/04_app/data/aglomerados.geojson", delete_dsn = TRUE)
write_csv(metricas, "src/04_app/data/metricas.csv")
st_write(radios_para_basemap, "data/processed/mapbox/radios_con_accesibilidad_para_basemap.geojson",
         delete_dsn = TRUE)
