library(tidyverse)
library(sf)

##################################################################################################
# Verificando la accesibilidad desde cada radio censal Urbano hasta el espacio verde más cercano #
##################################################################################################

# Cargamos espacios verdes y las isocronas
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

# Cargamos isocronas calculadas con el script src/02_b_processing_CABA/4a_estimar_isocronas_a_pie_10_minutos_CABA.R
isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp", 
                     stringsAsFactors = FALSE)

# Unificamos clusters y descartamos los que no alcanzan el umbral de area

umbral_area_m2 <- 5000

espacios_verdes <- espacios_verdes %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2) %>% 
    mutate(ha = as.numeric(st_area(.))/10000)


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
    write_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando_CABA_original_reprocesado.csv")
    
