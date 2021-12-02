library(tidyverse)
library(sf)
library(osrm)

################################################################################
# Estimar isocronas en bici desde cada radio censal de CABA - BASE (10 minutos)
################################################################################

options(osrm.server = "http://127.0.0.1:5000/")


#cargamos los radios
radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I)

#funcion isocronas
get_isocronas <- function(sf_object, minutos=10, resolucion=50, id_col = "id") {
    
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


# Dejamos la versión directa (tarda ~4 horas en una laptop 2018 con CPU i7 y 16GB RAM)
isocronas_bici <- get_isocronas(radios_CABA) %>% 
    select(id)
#______________________________________________________________________________

# Recalculamos manualmente las isocronas fallidas


#volvemos a procesar las isocronas que fallaron, modificaron el parametro de resolucion
radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(943, 994, 1092, 1163, 1200, 1241, 1296, 
                     1351, 1581, 1638, 1698, 1749, 1793)) %>% 
    st_transform(crs=4326)

# incrementamos "res" para encontrar las isocronas de los radios que fallaron, para luego unirlos
isocronas_fallidas <- get_isocronas(radios_fallidos, resolucion = 51, minutos = 10) %>% 
    select(id)

# ahora si unimos las isocronas fallidas a las generales
'%ni%' <- Negate('%in%') 

isocronas_merge <- isocronas_bici %>% 
    filter(id %ni% c(943, 994, 1092, 1163, 1200, 1241, 1296, 
                     1351, 1581, 1638, 1698, 1749, 1793)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)


# Guardamos los resultados
st_write(isocronas_merge, "data/processed/isocronas/bici/isocronas_10_min_en_bici_radios_CABA.shp")

