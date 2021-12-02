library(tidyverse)
library(sf)
library(osrm)
library(ggplot2)
library(classInt)

################################################################################
# Estimar isocronas a pie PONDERADAS POR INSEGURDAD RELATIVA desde cada radio censal de CABA
################################################################################

# entendiendo que los radios menos inseguros no se ven afectados en la disponibilidad a caminar, 
# mientras que los radios más inseguros ven la accesibilidad a los EV comprometida por las condiciones del entorno
# vamos a penalizar los entornos inseguros

options(osrm.server = "http://127.0.0.1:5000/")

#Cargamos los datos
radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I)

# cargamos los radios censales con su ponderador por inseguridad
radios_CABA_crime <- st_read("data/processed/GCABA/crime/radios_con_indice_crime_CABA.shp", stringsAsFactors = FALSE) %>% 
    as.data.frame() %>% 
    select (-geometry) %>% 
    rename (crime_index=crm_ndx)

#lo unimos a los datos base de los radios censales
radios_CABA <- radios_CABA %>% 
    left_join(radios_CABA_crime, by="id") %>% 
    select(id, geometry, crime_index)


# armamos quiebren para ponderar por grupos (categorizados)
jenks.brks <- classIntervals(radios_CABA_crime$crime_index,5)
jenks <- jenks.brks$brks

# pasamos los quiebres al índice
radios_CABA_crime <- radios_CABA_crime %>% 
    mutate(ponderador=cut(crime_index, breaks = jenks, labels=as.numeric(1:5), include.lowest = TRUE))

# Inspeccion visual
ggplot(radios_CABA_crime, aes(crime_index, fill=ponderador)) +
    geom_histogram(bins=50, alpha=.5, color="black")+
    sapply(jenks, function(x) geom_vline(aes(xintercept = x), color="black", size=1, linetype="dashed"))+
    scale_fill_viridis_d(option = "magma", direction = -1)+
    labs(fill="Grupo a penalizar",
         x="Crimen denunciado en el entorno del 10 minutos caminado/hab del radio censal", 
         y= "Frecuencia")+
    xlim(0,2)+
    theme_minimal()


# habiendo identificado cada grupo de grupo radio censal, procedemos a penalizarlo por inseguridad relativa

penalizacion <- 0.75 
radios_CABA_crime <- radios_CABA_crime %>% 
    mutate(tiempo = case_when((ponderador == 1) ~ 10,
                              (ponderador == 2) ~ 10-penalizacion,
                              (ponderador == 3) ~ 10-penalizacion*2,
                              (ponderador == 4) ~ 10-penalizacion*3,
                              (ponderador == 5) ~ 10-penalizacion*4))


# separamos en 5 set de datos para computar nuevamente las isocronas independientemente
radios_CABA_crime_1 <- filter(radios_CABA_crime, ponderador== 1)
radios_CABA_crime_2 <- filter(radios_CABA_crime, ponderador== 2)
radios_CABA_crime_3 <- filter(radios_CABA_crime, ponderador== 3)
radios_CABA_crime_4 <- filter(radios_CABA_crime, ponderador== 4)
radios_CABA_crime_5 <- filter(radios_CABA_crime, ponderador== 5)


# ya tenemos los radios censales asociados a su ponderador de inseguridad y penalizados
# ahora volveremos a calcular las isocronas pero esta ponderando por disponibilida a caminar 
# de acuerdo a las condiciones del entorno (insguridad y accidentes)

get_isocronas <- function(sf_object, minutos, resolucion=50, id_col = "id") {
    
    if (st_crs(sf_object)$epsg != 4326)  { st_transform(sf_object, 4326) }
    
    ids <- sf_object[[id_col]]
    
    isocrona_con_id <- function(id, origen) {
        print(paste("procesando ID", id))
        
        # Intentamos obtener isocrona
        resultado <- tryCatch(osrmIsochrone(origen, 
                                            breaks = c(0, minutos), 
                                            res = resolucion, 
                                            returnclass = "sf"),
                              error = function(e) {
                                  message(paste("falló ID", id, "\n"))
                                  message(paste("con error:", e))})
        
        # si falló el cálculo de la isocrona, creamos una vacía
        if(is.null(resultado) || nrow(resultado) == 0 ) { 
            resultado <- st_sf(id = NA, min = 0, max = minutos, center = minutos/2,
                               geometry = st_sfc(st_polygon(), crs = 4326))
        }
        
        # agregamos el ID del polígono al resultado
        cbind(id, resultado[-1])
    }
    
    
    sf_object %>% 
        st_centroid() %>% 
        st_coordinates() %>%
        {map2(.[,1], .[,2], c)} %>% 
        {map2(ids, ., isocrona_con_id)} %>% 
        reduce(rbind) 
}



# previamente a volver a procesarlas, cargamos las originales. 
# para filtrar las del grupo 1, ya que son las que tienen niveles de crimen "tolerables" 

isocronas <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp")

isocronas_1 <- isocronas %>% filter(id %in% radios_CABA_crime_1$id)



## REPROCESAMIENTO DE ISOCRONAS CATEGÓRICAS PONDERADAS

#ahora si, procesamos los otros 4 grupos, empezando por el grupo 2
radios_2 <- radios_CABA %>% 
    filter(id %in% (radios_CABA_crime_2$id))
isocronas_2<- get_isocronas(radios_2, minutos = 9.25)

radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(1163)) %>%
    st_transform(crs=4326)

# reprocesamos las que fallaron para el grupo 2
isocronas_fallidas <- get_isocronas(radios_fallidos, minutos = 9.25, resolucion = 55)

'%ni%' <- Negate('%in%') 
isocronas_2_merge <- isocronas_2 %>% 
    filter(id %ni% c(1163)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)

st_write(isocronas_2_merge, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-2.shp", delete_dsn = TRUE)


# grupo 3
radios_3 <- radios_CABA %>% 
    filter(id %in% (radios_CABA_crime_3$id))
isocronas_3 <- get_isocronas(radios_3, minutos = 8.5)

radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(1027, 1200, 1296, 1638, 1698, 1749, 1793)) %>%
    st_transform(crs=4326)

isocronas_fallidas <- get_isocronas(radios_fallidos, minutos = 8.5, resolucion = 55)

isocronas_3_merge <- isocronas_3 %>% 
    filter(id %ni% c(1027, 1200, 1296, 1638, 1698, 1749, 1793)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)

st_write(isocronas_3_merge, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-3.shp", delete_dsn = TRUE)


# grupo 4
radios_4 <- radios_CABA %>% 
    filter(id %in% (radios_CABA_crime_4$id))
isocronas_4 <- get_isocronas(radios_4, minutos = 7.75)

radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(1241, 1581)) %>%
    st_transform(crs=4326)

isocronas_fallidas <- get_isocronas(radios_fallidos, minutos = 7.75, resolucion = 55)

isocronas_4_merge <- isocronas_4 %>% 
    filter(id %ni% c(1241, 1581)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)

st_write(isocronas_4_merge, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-4.shp", delete_dsn = TRUE)


# grupo 5
radios_5 <- radios_CABA %>% 
    filter(id %in% (radios_CABA_crime_5$id))
isocronas_5 <- get_isocronas(radios_5, minutos = 7)

st_write(isocronas_5, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos-grupo-5.shp", delete_dsn = TRUE)


# GUARDADO FINAL:

isocronas_ponderadas <- bind_rows(isocronas_1, isocronas_2_merge, isocronas_3_merge, isocronas_4_merge, isocronas_5) %>% 
    st_transform(4326) %>% 
    select(id, geometry)
    arrange(id)

st_write(isocronas_ponderadas, "data/processed/isocronas/ponderadas-inseguridad/isocronas-caminando-ponderadas-45-segundos.shp", delete_dsn = TRUE)

