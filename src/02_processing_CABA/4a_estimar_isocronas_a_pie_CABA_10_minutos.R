library(tidyverse)
library(sf)
library(osrm)


###############################################################################
# Estimar isocronas a pie desde cada radio censal de CABA - BASE (10 minutos) #
###############################################################################

# ATENCIÓN: El ruteo requiere de una instancia local de OSRM. véase:
# https://rpubs.com/HAVB/osrm
options(osrm.server = "http://127.0.0.1:5000/")

# Cargamos radios censales de la Ciudad de Buenos Aires (CABA)
radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id = PAIS0210_I)

# Buscamos establecer el área que puede cubrirse a pie desde el centroide de cada
# radio censal en una distancia de 10 minutos, similar a la medotodología presente
# en "A WALK TO THE PARK? ASSESSING ACCESS TO GREEN AREAS IN EUROPE'S CITIES"
# https://ec.europa.eu/regional_policy/sources/docgener/work/2016_03_green_urban_area.pdf

# Funciones auxiliares
get_isocronas <- function(sf_object, minutos, resolucion, id_col = "id") {
    
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


isocronas <- get_isocronas(radios_CABA) %>% 
    select(id)

#-------------------------------------------------------------------------------

## RADIOS FALLIDOS

# Es posible observar que algunos radios fallaron en su procesamiento. Por este
# motivo, los recalculamos de forma manual, aumentando el parametro de resolución.
radios_fallidos <- radios_CABA %>% 
    filter(id %in% c(645, 648, 687, 994, 1027, 1041, 1092, 1163, 1200, 1241, 
                     1296, 1351, 1581, 1638, 1698, 1749, 1793, 3355, 3514)) %>%
    st_transform(crs = 4326)

# Incrementamos el parámetro de resolución para encontrar las isocronas de aquellos
# radios que fallaron, y, así, poder unirlos al resto. 
isocronas_fallidas <- get_isocronas(radios_fallidos, resolucion = 51, minutos = 10) %>% 
    select(id)

# Ahora si, unimos las isocronas fallidas a las generales.
'%ni%' <- Negate('%in%') 
isocronas_merge <- isocronas %>% 
    filter(id %ni% c(645, 648, 687, 994, 1027, 1041, 1092, 1163, 1200, 1241, 
                     1296, 1351, 1581, 1638, 1698, 1749, 1793, 3355, 3514)) %>% 
    bind_rows(isocronas_fallidas) %>% 
    arrange(id)


# Guardamos los resultados
st_write(isocronas_merge, "data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp")
