## Verificando accesibilidad desde cada radio censal Urbano hasta el espacio verde más cercano
library(tidyverse)
library(sf)

# Cargamos espacios verdes
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-ponderados.shp")

umbral_area_m2 <- 5000

# Unificamos clusters y descartamos los que no alcanzan el umbral de area

espacios_verdes <- espacios_verdes %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2) %>% 
    mutate(ha = as.numeric(st_area(.))/10000)

# Cargamos isocronas calculadas con el script src/02_processing/1_estimar_isocronas_a_pie.R
isocronas <- st_read("data/processed/isocronas/bici/ponderadas-inseguridad/isocronas-bici-ponderadas-45-segundos.shp", 
                     stringsAsFactors = FALSE)

# Unificamos proyecciones
isocronas <- st_transform(isocronas, st_crs(espacios_verdes)) %>% 
    st_make_valid()

# Identificamos cantidad y area total de los espacios verdes dentro de la cobertura de cada isocrona
accesibilidad <- st_join(isocronas, espacios_verdes) %>% 
    group_by(id) %>% 
    summarise(n = n(),
              total_ha = sum(ha, na.rm = TRUE)) %>% 
    mutate(n = ifelse(total_ha == 0, 0, n)) %>% 
    st_set_geometry(NULL)

# Guardamos resultados

accesibilidad  %>% 
    write_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_bici_CABA_ponderados-45-segundos.csv")
    
