library(tidyverse)
library(sf)
library(lwgeom)
sf::sf_use_s2(FALSE) # Apagamos la geometría esférica.


##################################################################################################################
# Extraer métricas de accesibilidad a espacios verdes - a menos de 10 minutos a pie - ponderadas por inseguridad #
##################################################################################################################


# DESCRIPCIÓN


# Cargamos las bases de datos.
# 1. Límite de la Ciudad de Buenos Aires (CABA).
caba_limite <- st_read("data/raw/OSM/limite_CABA.shp")

# 2. Comunas de la CABA.
comunas <- st_read("data/raw/GCABA/Comunas/comunas.geojson")

# 3. Radios censales de la CABA.
radios_caba <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    transmute(id = PAIS0210_I,
              tot_pob = TOT_POB)

# 4. Radios censales (con y sin acceso a espacios verdes).
accesibilidad  <- read_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando_CABA_ponderados-45-segundos.csv") %>% 
    mutate(situacion = ifelse(total_ha > 0, "con_acceso", "sin_acceso"))

# 5. Espacios Verdes
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp") %>% 
    group_by(clstr_d) %>% # Unimos los clusters.
    summarise(area_m2 = sum(area_m2)) # Sumamos su área.

# Juntamos todo.
base_combinada <- radios_caba %>% 
    left_join(accesibilidad, by = "id") %>% 
    # Calculamos los m2 per cápita y los dividimos en deciles.
    mutate(m2_per_capita = (total_ha/tot_pob) * 10000, # Convertimos a las ha a m2.
           decil_m2_per_capita = .bincode(m2_per_capita,
                                          breaks = quantile(m2_per_capita, probs = seq(0, 1, 1/10), na.rm = TRUE),
                                          include.lowest = TRUE))

# Veamos los m2 de accesibilidad per capita por deciles.
quantile(base_combinada$m2_per_capita, probs = seq(0, 1, 1/10), na.rm = TRUE)

# Ahora, veamos cuanta población se encuentra incluida en cada decil de m2 accesibles.
poblacion_q <- base_combinada %>% 
    as.data.frame() %>% 
    group_by(decil_m2_per_capita) %>% 
    summarise(poblacion = sum(tot_pob)) %>% 
    mutate(porcentaje_caba = poblacion/sum(radios_caba$tot_pob, na.rm = TRUE)*100) 

# Graficamos.
ggplot() +
    geom_sf(data = caba_limite, color = "black", size = 1, fill = NA)+
    geom_sf(data = base_combinada, aes(fill = decil_m2_per_capita), color = "grey85", size = .1)+
    geom_sf(data = comunas, fill = NA, size = .1, color = "black", alpha = .2)+
    geom_sf(data = espacios_verdes, fill = "#85C285", color = "#004225", size = .1)+
    scale_fill_gradient(low = "white", high = "#5a2163")+
    labs(fill=" Decil m2 verde \n\ per cápita")+
    theme_void()


##################################################################################
# ACÄ HAY ALGO QUE NO ME CIERRA, EN ESTA MAPA, A DIFERENCIA AL DEL SCRIPT ANTERIOR
# LA INTENSIDAD DEL VIOLETA ES MAYOR, PESE A QUE LA INSEGURIDAD DEBERÍA ACTUAR AL REVÉS
################################################################################

# Guardamos.
st_write(base_combinada, "data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA.shp", delete_dsn = TRUE)

