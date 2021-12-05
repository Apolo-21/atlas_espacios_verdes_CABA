########################################################
# Extraer métricas de accesibilidad a espacios verdes, base sin ponderacion
########################################################

library(tidyverse)
library(sf)
library(lwgeom)
sf::sf_use_s2(FALSE) #apagamos la geometría esférica


# Cargamos las bases de datos
CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    st_difference()

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(CABA_limite)

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I) %>% 
    select(id, TOT_POB)

# Unificamos clusters y calculamos los m2
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2))


accesibilidad  <- read_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando_CABA_original_reprocesado.csv") %>% 
    mutate(situacion = ifelse(total_ha > 0, "con_acceso", "sin_acceso"))


# Juntamos todo
base_combinada <- radios_CABA %>% 
    left_join(accesibilidad) %>% 
    # calculamos los m2 per cápita y los clasificamos en sus deciles
    mutate(m2_per_capita = (total_ha/TOT_POB)*10000, #lo pasamos a
               decil_m2_per_capita=.bincode(m2_per_capita, breaks = quantile(m2_per_capita, probs = seq(0, 1, 1/10), na.rm = TRUE),include.lowest = TRUE))

# Vemos la distribucin por quintiles de accesibilidad per capita
quantile(base_combinada$m2_per_capita, probs = seq(0, 1, 1/10), na.rm = TRUE)


#graficamos

ggplot() +
    geom_sf(data=CABA_limite, color="black", size=1, fill=NA)+
    geom_sf(data=base_combinada, aes(fill=decil_m2_per_capita), color="grey85", size=.1)+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.2)+
    geom_sf(data=espacios_verdes_cualificado, fill="#85C285", color="#004225", size=.1)+
    scale_fill_gradient(low="white", high = "#5a2163")+
    labs(fill=" Decil m2 verde \n\ per cápita")+
    theme_void()

st_write(base_combinada, "data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA_orginal.shp", delete_dsn = TRUE)
