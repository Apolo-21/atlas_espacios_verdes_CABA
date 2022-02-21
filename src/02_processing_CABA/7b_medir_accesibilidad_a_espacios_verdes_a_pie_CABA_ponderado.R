library(tidyverse)
library(sf)


##################################################################################
# Accesibilidad a pie desde cada radio censal hasta el espacio verde más cercano #
# ponderada por inseguridad y disponibilidad (enrejamiento) del mismo            #
##################################################################################

# El siguiente script busca establecer el nº de espacios verdes y la superficie verde 
# accesible desde cada radio censal en un entorno caminable de 10 minutos, ponderando
# por inseguridad y dsiponibilidad del espacio verde.

# Cargamos los espacios verdes.
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-ponderados.shp")

# Establecemos el umbral de área para considerar los EV.
umbral_area_m2 <- 5000

# Unificamos clusters y descartamos los que no alcanzan el umbral de área.
espacios_verdes <- espacios_verdes %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2) %>% 
    mutate(ha = as.numeric(st_area(.))/10000)

# Cargamos isocronas poderadas por inseguridad del entorno caminable (calculadas
# en el script "06_a".
isocronas <- st_read("data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos.shp", 
                     stringsAsFactors = FALSE)

# Unificamos proyecciones
isocronas <- isocronas %>% 
    st_transform(st_crs(espacios_verdes)) %>% 
    st_make_valid()

# Identificamos la cantidad y la superficie total de los EV que cubre cada isocrona.
accesibilidad <- st_join(isocronas, espacios_verdes) %>% 
    group_by(id) %>% 
    summarise(n = n(),
              total_ha = sum(ha, na.rm = TRUE)) %>% 
    mutate(n = ifelse(total_ha == 0, 0, n)) %>% 
    st_set_geometry(NULL)


# Guardamos los resultados.
write_csv(accesibilidad,"data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando_CABA_ponderados-45-segundos.csv")
