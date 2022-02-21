library(tidyverse)
library(sf)


##################################################################################
# Accesibilidad a pie desde cada radio censal hasta el espacio verde más cercano #
##################################################################################

# El siguiente script busca establecer el nº de espacios verdes y la superficie verde 
# accesible desde cada radio censal en un entorno caminable de 10 minutos.

# Cargamos los espacios verdes.
espacios_verdes <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

# Cargamos las isocronas (a pie).
isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp", 
                     stringsAsFactors = FALSE)

# Para medir la accesibilidad a espacios verdes (EV), el Atlas de Espacios Verdes
# de Bunge & Born solo computa como relevantes aquellos EV mayores a 5000 m2. La 
# justificación de está decisión subyace sobre la idea de que EV de menor tamaño
# no garantizan una accesibilidad de calidad a los mismos.
umbral_area_m2 <- 5000 

# Unificamos clusters y descartamos aquellos que no alcanzan el umbral de área.
espacios_verdes <- espacios_verdes %>% 
    group_by(clstr_d) %>% 
    summarise(area_m2 = sum(area_m2)) %>% 
    filter(area_m2 >= umbral_area_m2) %>% 
    mutate(ha = as.numeric(st_area(.))/10000) # En la Ciudad de Buenos Aires existen 258 EV que permiten una accesibilidad de calidad.

# Unificamos proyecciones.
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
write_csv(accesibilidad, "data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando_CABA_original_reprocesado.csv")
