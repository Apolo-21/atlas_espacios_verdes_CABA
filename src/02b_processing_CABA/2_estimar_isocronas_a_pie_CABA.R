###########################################################
# Estimar isocronas a pie desde cada radio censal de CABA #
###########################################################

# ATENCION #
# El ruteo requiere de una instancia local de OSRM
# véase:
# https://rpubs.com/HAVB/osrm

library(tidyverse)
library(sf)
library(osrm)

options(osrm.server = "http://127.0.0.1:5000/")


## Cargamos radios censales urbanos usados para la EPH

# Retenemos sólo radios de CABA, en proyeccion Mercator como le gusta a OSRM
radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I)

# Buscamos establecer el área que puede cubrirse a pie desde el centroide de cada radio censal con una
# caminada de 10 minutos, similar a la medotodología en 
# A WALK TO THE PARK? ASSESSING ACCESS TO GREEN AREAS IN EUROPE'S CITIES
# https://ec.europa.eu/regional_policy/sources/docgener/work/2016_03_green_urban_area.pdf

# Funciones auxiliares
# 
get_isocronas <- function(sf_object, minutos, resolucion, id_col = "id") {
    
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


procesar_isocronas_aglomerado <- function(codigo, dest = "data/processed/isocronas") {
    
    radios_ciudades %>% 
        filter(codaglo == codigo) %>% 
        get_isocronas() %>% 
        st_write(paste0(dest, "/", codigo, ".geojson"), delete_dsn = TRUE)
}

procesar_isocronas_depto <- function(codigo, dest = "data/processed/isocronas") {
    
    radios_ciudades %>% 
        filter(coddepto == codigo) %>% 
        get_isocronas() %>% 
        st_write(paste0(dest, "/", codigo, ".geojson"), delete_dsn = TRUE)
}


# En la práctica lo hicimos de a "cachos" porque los 26K radios urbanos toman como 14 horas en mi laptop... 
# Mejor ir procesandolos de a uno, y si llega a falla algun departamento se preservan los logrados antes
# (HAVB)

# radios_ciudades %>% 
#     pull(coddepto) %>% 
#     unique() %>% 
#     #sort() %>%  # para testing
#     #tail(2) %>%  # para testing
#     # Solo queremos el efecto secundario (guardar archivo .geojson con isocronas en disco)
#     walk(procesar_isocronas_depto)
# 
# 
# 
# list.files("data/processed/isocronas", full.names = TRUE) %>% 
#     map(st_read, stringsAsFactors = FALSE) %>% 
#     reduce(rbind) %>%
#     select(id) %>% 
#     st_write("data/processed/isocronas/isocronas_10_min_a_pie_radios_urbanos.shp",
#              delete_dsn = TRUE)


# Dejamos la versión directa (tarda ~4 horas en una laptop 2018 con CPU i7 y 16GB RAM)
isocronas <- get_isocronas(radios_CABA) %>% 
    select(id)


#RADIOS FALLIDOS

radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(645, 648, 687, 994, 1027, 1041, 1092, 1163, 1200, 1241, 
                     1296, 1351, 1581, 1638, 1698, 1749, 1793, 3355, 3514)) %>%
    st_transform(crs=4326)


#inspeccion visual
ggplot()+
    geom_sf(data=radios_CABA, fill="white", color="gray80")+
    geom_sf(data=radios_fallidos, fill="red")+
    theme_minimal()

# incrementamos "res" para encontrar las isocronas de los radios que fallaron, para luego unirlos
isocronas_fallidas <- get_isocronas(radios_fallidos, resolucion = 51, minutos = 10) %>% 
    select(id)

# inspección visual
ggplot()+
    geom_sf(data=isocronas)+
    geom_sf(data=isocronas_fallidas, fill="red")


# ahora si unimos las isocronas fallidas a las generales
'%ni%' <- Negate('%in%') 
isocronas_merge <- isocronas %>% 
    filter(id %ni% c(645, 648, 687, 994, 1027, 1041, 1092, 1163, 1200, 1241, 
                     1296, 1351, 1581, 1638, 1698, 1749, 1793, 3355, 3514)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)


# Guardamos los resultados
st_write(isocronas_merge, "data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp")

