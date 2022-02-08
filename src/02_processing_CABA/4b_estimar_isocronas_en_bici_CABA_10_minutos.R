library(tidyverse)
library(sf)
library(osrm)


#################################################################################
# Estimar isocronas en bici desde cada radio censal de CABA - BASE (10 minutos) #
#################################################################################

# ATENCIÓN: El ruteo requiere de una instancia local de OSRM. véase:
# https://rpubs.com/HAVB/osrm
options(osrm.server = "http://127.0.0.1:5000/")

# Repetimos el mismo proceso que el script anterior, pero ahora calculamos la distancia
# recorrida en 10 minutos en bicicleta.

# Cargamos los radios de la Ciudad de Buenos Aires.
radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=  PAIS0210_I)

# Función isocronas.
get_isocronas <- function(sf_object, minutos = 10, resolucion = 50, id_col = "id") {
    
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
        
        # Si falla el cálculo de la isocrona, creamos una vacía en su lugar.
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
        {map2(.[,1], .[,2], c)} %>% 
        {map2(ids, ., isocrona_con_id)} %>% 
        reduce(rbind) 
}


# Dejamos la versión directa (tarda ~4 horas en una laptop 2018 con CPU i7 y 16GB RAM)
isocronas_bici <- get_isocronas(radios_CABA) %>% 
    select(id)

#-------------------------------------------------------------------------------
# QUË VA A PENSAR CYNTHIA SI LEE ESTO? QUË SOMOS UNOS DESAGRADECIDOS PORQUE NO
# USAMOS LAS PCS QUE NOS COMPRÖ? NOS VA A DESHEREDAR. :O 
#-------------------------------------------------------------------------------


## Recalculamos manualmente las isocronas fallidas.

# Volvemos a procesar las isocronas que fallaron. Para ello, modificamos su parámetro
# de resolución.
radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(943, 994, 1092, 1163, 1200, 1241, 1296, 
                     1351, 1581, 1638, 1698, 1749, 1793)) %>% 
    st_transform(crs = 4326)

# Incrementamos el parámetro de resolución para encontrar las isocronas de aquellos
# radios que fallaron, y, así, poder unirlos al resto. 
isocronas_fallidas <- get_isocronas(radios_fallidos, resolucion = 51, minutos = 10) %>% 
    select(id)

# Ahora si, unimos las isocronas fallidas a las generales.
'%ni%' <- Negate('%in%') 

isocronas_merge <- isocronas_bici %>% 
    filter(id %ni% c(943, 994, 1092, 1163, 1200, 1241, 1296, 
                     1351, 1581, 1638, 1698, 1749, 1793)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)


# Guardamos los resultados
st_write(isocronas_merge, "data/processed/isocronas/bici/isocronas_10_min_en_bici_radios_CABA.shp")
