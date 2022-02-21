library(tidyverse)
library(sf)
library(osrm)
library(classInt)

#####################################################################
# Ponderar isocronas por inseguridad relativa del entorno caminable #
#####################################################################

# El actual trabajo se construye sobre la premisa de que la (in)seguridad impacta
# sobre la predisposición de las personas a caminar (para más información, ver informe
# final). En este marco, cuanto más inseguro sea un entorno (radio censal), mayor
# la reticencia de las personas a desplazarse a pie y más comprometida la accesibilidad
# de estos a espacios verdes,

options(osrm.server = "http://127.0.0.1:5000/")

# Cargamos los datos:
# 1. Radios censales.
radios_caba <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>% 
    st_transform(4326) %>% 
    rename(id = PAIS0210_I)

# 2. Radios censales ponderados por inseguridad.
radios_caba_crimen <- st_read("data/processed/GCABA/crime/radios_con_indice_crime_CABA.shp", stringsAsFactors = FALSE) %>% 
    st_set_geometry(NULL) %>% 
    rename("crime_index" = crm_ndx)

# Unimos ambos datasets.
radios_caba <- radios_caba %>% 
    left_join(radios_caba_crimen, by = "id") %>% 
    select(id, crime_index) 



# NO ENTIENDO ESTO? PARA QUÉ LE SACAMOS LA GEOMETRIA A LOS RADIOS PONDERADOS SI DESP SE LA VOLVEMOS A PONER Y NO AGREGAMOS NADA????


# Establecemos quiebres con el objetivo de clasificar a los radios censales en grupos.
jenks.brks <- classIntervals(radios_caba_crimen$crime_index, 5)
jenks <- jenks.brks$brks

# Introduciemos los quiebres al índice de criminalidad (inseguridad del entorno).
radios_caba_crimen <- radios_caba_crimen %>% 
    mutate(ponderador = cut(crime_index, breaks = jenks, labels = as.numeric(1:5), include.lowest = TRUE))

# Inspección visual.
ggplot(radios_caba_crimen, aes(crime_index, fill = ponderador))+
    geom_histogram(bins = 50, alpha = .5, color = "black")+
    sapply(jenks, function(x) geom_vline(aes(xintercept = x), color = "black", size = 1, linetype = "dashed"))+
    scale_fill_viridis_d(option = "magma", direction = -1)+
    labs(fill = "Grupo a penalizar",
         x = "Delitos denunciados en un entorno caminable de 10 minutos desde el centroide del radio censal", 
         y = "Frecuencia")+
    xlim(0, 2)+
    theme_minimal()

# Luego de haber identificado el grupo al que pertenece cada radio censal, procedemos
# a penalizar su caminabilidad por inseguridad relativa del entorno.
penalizacion <- 0.75 # DE DONDE SALE ESTE Nº???

radios_caba_crimen <- radios_caba_crimen %>% 
    mutate(tiempo = case_when((ponderador == 1) ~ 10,
                              (ponderador == 2) ~ 10 - penalizacion,
                              (ponderador == 3) ~ 10 - penalizacion*2,
                              (ponderador == 4) ~ 10 - penalizacion*3,
                              (ponderador == 5) ~ 10 - penalizacion*4))


# Separamos en 5 set de datos para computar nuevamente las isocronas independientemente.
radios_caba_crimen_1 <- filter(radios_caba_crimen, ponderador == 1)
radios_caba_crimen_2 <- filter(radios_caba_crimen, ponderador == 2)
radios_caba_crimen_3 <- filter(radios_caba_crimen, ponderador == 3)
radios_caba_crimen_4 <- filter(radios_caba_crimen, ponderador == 4)
radios_caba_crimen_5 <- filter(radios_caba_crimen, ponderador == 5)


# Ya tenemos los radios censales asociados a su ponderador de inseguridad y penalizados.
# Ahora, volveremos a calcular las isocronas pero ponderando por disposición a caminar 
# de acuerdo a las condiciones del entorno (insguridad y accidentes).
get_isocronas <- function(sf_object, minutos, resolucion = 50, id_col = "id") {
    
    if (st_crs(sf_object)$epsg != 4326)  { st_transform(sf_object, 4326) }
    
    ids <- sf_object[[id_col]]
    
    isocrona_con_id <- function(id, origen) {
        print(paste("procesando ID", id))
        
        # Intentamos obtener la isocrona.
        resultado <- tryCatch(osrmIsochrone(origen, 
                                            breaks = c(0, minutos), 
                                            res = resolucion, 
                                            returnclass = "sf"),
                              error = function(e) {
                                  message(paste("falló ID", id, "\n"))
                                  message(paste("con error:", e))})
        
        # Si falló el cálculo de la isocrona, creamos una vacía.
        
        if(is.null(resultado) || nrow(resultado) == 0 ) { 
            resultado <- st_sf(id = NA, min = 0, max = minutos, center = minutos/2,
                               geometry = st_sfc(st_polygon(), crs = 4326))
        }
        
        # Agregamos el ID del polígono al resultado.
        
        cbind(id, resultado[-1])
    }
    
    
    sf_object %>% 
        st_centroid() %>% 
        st_coordinates() %>%
        {map2(.[, 1], .[, 2], c)} %>% 
        {map2(ids, ., isocrona_con_id)} %>% 
        reduce(rbind) 
}


# Previo a volver a procesar las isocronas, cargamos las originales con el fin de
# filtrar  aquellas pertenecientes al GRUPO 1, las cuales poseen niveles de crimen
# "tolerables".
isocronas_1 <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
    filter(id %in% radios_caba_crimen_1$id)


# Reprocesamiento de isocronas categóricas ponderadas --------------------------

# Ahora si, intervengamos los demás grupos, empezando por el GRUPO 2.
radios_2 <- radios_caba %>% 
    filter(id %in% (radios_caba_crimen_2$id))
isocronas_2 <- get_isocronas(radios_2, minutos = 9.25)

# Reprocesamos las que fallaron para el GRUPO 2
#isocronas_fallidas <- get_isocronas(radios_fallidos, minutos = 9.25, resolucion = 55)

#'%ni%' <- Negate('%in%') 
#isocronas_2_merge <- isocronas_2 %>% 
#    filter(id %ni% c(1163)) %>% 
#    bind_rows(isocronas_fallidas) %>% 
#    arrange(id)



####### Y ESTO????



st_write(isocronas_2, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-2.shp", delete_dsn = TRUE)


## GRUPO 3.
radios_3 <- radios_caba %>% 
    filter(id %in% (radios_caba_crimen_3$id))
isocronas_3 <- get_isocronas(radios_3, minutos = 8.5)

radios_fallidos <- radios_caba %>% 
    filter(id %in% c(1027, 1200, 1296, 1638, 1698, 1749, 1793)) %>%
    st_transform(crs = 4326)

#isocronas_fallidas <- get_isocronas(radios_fallidos, minutos = 8.5, resolucion = 55)

#isocronas_3_merge <- isocronas_3 %>% 
#    filter(id %ni% c(1027, 1200, 1296, 1638, 1698, 1749, 1793)) %>% 
#    bind_rows(isocronas_fallidas) %>% 
#    arrange(id)

st_write(isocronas_3, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-3.shp", delete_dsn = TRUE)


## GRUPO 4.
radios_4 <- radios_caba %>% 
    filter(id %in% (radios_caba_crimen_4$id))
isocronas_4 <- get_isocronas(radios_4, minutos = 7.75)

#radios_fallidos <- radios_CABA %>% 
#    filter(id %in% c(1241, 1581)) %>%
#    st_transform(crs=4326)

#isocronas_fallidas <- get_isocronas(radios_fallidos, minutos = 7.75, resolucion = 55)

#isocronas_4_merge <- isocronas_4 %>% 
#    filter(id %ni% c(1241, 1581)) %>% 
#    bind_rows(isocronas_fallidas) %>% 
#    arrange(id)



################################################################################
# HAY QUE LIMPIAR TODAS ESTAS COSAS SI NO SIRVEN
################################################################################



st_write(isocronas_4, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-4.shp", delete_dsn = TRUE)


# GRUPO 5
radios_5 <- radios_CABA %>% 
    filter(id %in% (radios_caba_crimen_5$id))
isocronas_5 <- get_isocronas(radios_5, minutos = 7)

st_write(isocronas_5, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-5.shp", delete_dsn = TRUE)

# Unismos todas las isocronas en un solo dataset y gurdamos.
isocronas_ponderadas <- bind_rows(isocronas_1, isocronas_2, isocronas_3, isocronas_4, isocronas_5) %>% 
    st_transform(4326) %>% 
    select(id, geometry) %>% 
    arrange(id)


st_write(isocronas_ponderadas, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos.shp", delete_dsn = TRUE)

