## Verificando accesibilidad desde cada radio censal Urbano hasta el espacio verde más cercano
library(tidyverse)
library(sf)

# Cargamos espacios verdes
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-ponderados.shp")


# Umbral de corte para áreas de espacios verdes
# Consideramos un mínimo de media hectárea, o 5000 m2, 
# siguiendo los lineamientos de los Indicadores Europeos de Sustentabilidad
# (https://www.gdrc.org/uem/footprints/eci_final_report.pdf). 
# que miden el porcentaje de habitantes que reside a menos de 300 metros lineales de un espacio abierto y público 
# de al menos media hectárea.

umbral_area_m2 <- 5000

# Unificamos clusters y descartamos los que no alcanzan el umbral de area

espacios_verdes <- espacios_verdes %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2) %>% 
    mutate(ha = as.numeric(st_area(.))/10000)

# Cargamos isocronas calculadas con el script src/02_processing/1_estimar_isocronas_a_pie.R
isocronas <- st_read("data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos.shp", 
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
    write_csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando_CABA_ponderados-45-segundos.csv")
    
