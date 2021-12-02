########################################################
# Extraer métricas de accesibilidad a espacios verdes  #
########################################################

library(tidyverse)
library(sf)
library(lwgeom)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica


radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I) %>% 
    select(id, TOT_POB)

# Umbral de corte para áreas de espacios verdes
# Consideramos un mínimo de media hectárea, o 5000 m2, 
# siguiendo los lineamientos de los Indicadores Europeos de Sustentabilidad
# (https://www.gdrc.org/uem/footprints/eci_final_report.pdf). 
# que miden el porcentaje de habitantes que reside a menos de 300 metros lineales de un espacio abierto y público 
# de al menos media hectárea.

umbral_area_m2 <- 5000

# Unificamos clusters y descartamos los que no alcanzan el umbral de area

espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-ponderados.shp") %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2)

accesibilidad  <- read_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_bici_CABA_ponderados-45-segundos.csv") %>% 
    mutate(situacion = ifelse(total_ha > 0, "con_acceso", "sin_acceso"))

########################################
# Métrica "sofisticada": accesibilidad #
########################################

# Juntamos todo

base_combinada <- radios_CABA %>% 
    left_join(accesibilidad) %>% 
    # calculamos los m2 per cápita y los clasificamos en sus deciles
    mutate(m2_per_capita = (total_ha/TOT_POB)*10000, #lo pasamos a
           decil_m2_per_capita=.bincode(m2_per_capita, breaks = quantile(m2_per_capita, probs = seq(0, 1, 1/10), na.rm = TRUE),include.lowest = TRUE))

quantile(base_combinada$m2_per_capita, probs = seq(0, 1, 1/10), na.rm = TRUE)


#graficamos

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    st_difference()

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>% 
    st_transform(4326)
comunas <- comunas %>% 
    st_intersection(CABA_limite)

espacios_verdes_originales <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
    st_transform(4326)


ggplot() +
    geom_sf(data=CABA_limite, color="black", size=1, fill=NA)+
    geom_sf(data=base_combinada, aes(fill=decil_m2_per_capita))+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.1)+
    geom_sf(data=espacios_verdes_originales, fill="#69b166", color="black", size=.1)+
    scale_fill_gradient(low="white", high = "#8F00FF")+
    labs(fill=" Decil m2 verde \n\ per cápita")+
    theme_void()


ggplot()+
    geom_sf(data=base_combinada, aes(fill=situacion)) +
    geom_sf(data=espacios_verdes, fill="darkgreen")+
    theme_void()

st_write(base_combinada, "presentation/data/accesibilidad_espacios_verdes_bici_CABA.shp", delete_dsn = TRUE)
