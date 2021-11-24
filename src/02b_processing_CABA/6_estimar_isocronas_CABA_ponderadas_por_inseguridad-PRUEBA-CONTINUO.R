library(tidyverse)
library(sf)
library(osrm)
library(ggplot2)

options(osrm.server = "http://127.0.0.1:5000/")


radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I)

# cargamos los radios censales con su ponderador por inseguridad
radios_CABA_crime <- st_read("data/processed/GCABA/crime/radios_con_indice_crime_CABA.shp", stringsAsFactors = FALSE) %>% 
    as.data.frame() %>% 
    select(-geometry, crm_ndx) %>% 
    mutate(crime_pond=crm_pnd)

#lo unimos a los datos base de los radios censales
radios_CABA <- radios_CABA %>% 
    left_join(radios_CABA_crime, by="id") %>% 
    select(id, geometry, crime_pond)

##
for (i in radios_CABA_crime$crime_pond) {
    ponderador=round(i,2)
    
    print(ponderador)
}
valor <- function(x) x
valor(radios_CABA_crime$crime_pond)
##

# ya tenemos los radios censales asociados a su ponderador de inseguridad
# ahora volveremos a calcular las isocronas pero esta ponderando por disponibilida a caminar 
# de acuerdo a las condiciones del entorno (insguridad y accidentes)

get_isocronas <- function(sf_object, minutos=10, pond_col="crime_pond", resolucion = 50, id_col = "id") {
    
    if (st_crs(sf_object)$epsg != 4326)  { st_transform(sf_object, 4326) }
    
    ids <- sf_object[[id_col]]
    
    pond_cols <- sf_object[[pond_col]]

    isocrona_con_id <- function(id, origen, crime_pond) {
        print(paste("procesando ID", id, "- ponderador: ", crime_pond))

        # Intentamos obtener isocrona

        resultado <- tryCatch(osrmIsochrone(origen, 
                                            breaks = c(0, minutos*crime_pond), 
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
        cbind(id, resultado[-1], crime_pond)
        }


    sf_object %>% 
        st_centroid() %>% 
        st_coordinates() %>%
        {map2(.[,1], .[,2], .[,3], c)} %>% 
        {map2(..ids, ..pond_cols, ..isocrona_con_id,...)} %>% 
        reduce(rbind) 

}

isocronas <- get_isocronas(radios_CABA_prueba) %>% 
    select(id)


radios_CABA_prueba <- radios_CABA %>% 
    filter(id %in% c(500, 1250, 1750, 2000)) %>% 
    mutate(crime_pond = case_when((id <= 1500) ~ 1,
                                  (id > 1500) ~ crime_pond))
##
ponderador <- function(x) x
lista<-ponderador(radios_CABA_crime$crime_pond)
##


for (i in radios_CABA_crime$crime_pond) {
    ponderador=round(i,2)
}    
    print(ponderador)
    
    


    
    
isocronas_originales <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
    filter(id %in% c(500, 1250, 1750, 2000))

ggplot()+
    geom_sf(data=radios_CABA_prueba, fill="grey")+
    geom_sf(data=isocronas, fill="red", alpha=.5, color=NA)+
    geom_sf(data=isocronas %>% filter(id<=1500), fill="blue", alpha=.5, color=NA)+
    geom_sf(data=isocronas_originales, fill=NA, color="blue")
    
