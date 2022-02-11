library(tidyverse)
library(sf)
sf::sf_use_s2(FALSE) # Apagamos la geometría esférica.


##########################################################################################
# Estimar inseguridad del entorno caminable a 10 minutos desde cada radio censal de CABA #
##########################################################################################


#-------------------------------------------------------------------------------
# Luis, te animas a escrbir algo breve acá de por qué nos interesa esto y qué hace
# este script al respecto?
#-------------------------------------------------------------------------------


# Carga de bases de datos.
# Cargamos los límites de CABA de OSM
CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    st_difference()

comunas <- st_read("data/raw/GCABA/Comunas/comunas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(CABA_limite)

delito <- read_csv("data/raw/GCABA/delito/delitos_2020.csv") # ACTUALIZAMOS X DELITOS 2021????

isocronas_CABA <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    select(id)

radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") 

#Convertivos en espacial la base de delitos y lo intersectamos con la geometría de CABA (que caen fuera, por error de carga)

delito_tipo <- delito %>% 
    group_by(tipo) %>% 
    summarise(cantidad=n())

# vamos a corregir valores que no tiene los decimales cargados
delito <- delito %>% 
    filter(tipo != "Homicidio") %>% 
    drop_na(latitud, longitud) %>% 
    mutate(latitud = case_when(
                (latitud < -40) ~ latitud/1000,
                (latitud >= -40 ~ latitud)),
           longitud = case_when(
                 (longitud < -60) ~ longitud/1000,
                 (longitud >= -60 ~ longitud)))
    
#ahora sí vamos a levantarlo geográficamente
delito <- delito %>% 
    st_as_sf(coords = c("longitud", "latitud"), crs = 4326) %>% 
    st_intersection(CABA_limite) %>% 
    mutate(lat = unlist(map(geometry,2)),
           long = unlist(map(geometry,1)))

#_______________________________________________________________________________
#inspección visual

ggplot() +
    geom_sf(data=CABA_limite, fill="white", size=1)+
    stat_density_2d(data=delito, aes(x=long, y=lat, fill = stat(level)), color = "grey20", size=0.8, linetype="dashed", geom = "polygon")+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.3)+
    scale_fill_viridis_c(direction = -1, option = "magma")+
    labs(fill=" Crimen denunciado\n\ 2020")+
    theme_void()

#_______________________________________________________________________________

## Vamos a pasar la info del mapa de calor a los radios censales

# unimos espacialmente las iscronas base (10 mins) con los delitos, para ver su incidencia en el entorno
iso_delito <- st_join (delito, isocronas_CABA)

iso_delito_id <- iso_delito %>% 
    as.data.frame() %>% 
    select(id, tipo) %>% 
    group_by(id, tipo) %>%
    summarise(cant_delito_id=n())

# cargamos los radios censales, para normalizar por población
# le agregamos la info de crimen por radio censal

radios_df <- radios %>% 
    as.data.frame() %>% 
    select(PAIS0210_I, TOT_POB) %>% 
    rename(id=PAIS0210_I) %>% 
    replace(is.na(.), 0)

iso_delito_id <- iso_delito_id %>% 
    left_join(radios_df, by="id")

#_______________________________________________________________________________

# CREACION DE INDICES POR TIPO DE DELITO
#_______________________________________

# iso_hurto
iso_hurto <- iso_delito_id %>% 
    filter(tipo=="Hurto (sin violencia)")

iso_hurto<- iso_hurto %>% 
    mutate(hurto_index=cant_delito_id/TOT_POB) %>% #incidencia del radio censal s/isocrona 
    transmute(id, hurto_index) 

#reemplazamos los valores infinitos producto de dividir por 0, por 0
iso_hurto <- do.call(data.frame,lapply(iso_hurto, function(x) replace(x, is.infinite(x),0)))


#_______________________________________
# índice de lesiones
iso_lesiones <- iso_delito_id %>% 
    filter(tipo=="Lesiones")

iso_lesiones<- iso_lesiones %>% 
    mutate(lesiones_index=cant_delito_id/TOT_POB,
           lesiones_index=replace_na(lesiones_index, 0)) %>% #incidencia del radio censal s/isocrona 
    transmute(id, lesiones_index)

iso_lesiones <- do.call(data.frame,lapply(iso_lesiones, function(x) replace(x, is.infinite(x),0)))


#_______________________________________
# índice de robos
iso_robo <- iso_delito_id %>% 
    filter(tipo=="Robo (con violencia)")

iso_robo<- iso_robo %>% 
    mutate(robo_index=cant_delito_id/TOT_POB, #incidencia del radio censal s/isocrona 
           robo_index=replace_na(robo_index, 0)) %>% 
    transmute(id, robo_index)

iso_robo <- do.call(data.frame,lapply(iso_robo, function(x) replace(x, is.infinite(x),0)))



# RADIOS ACTUALIZADOS CABA

radios <- radios %>%
    st_transform(4326) %>% 
    rename(id=PAIS0210_I) %>% 
    arrange(id)

# Le agregamos los indices de inseguridad desagregados
radios_CABA_crime <- radios %>% 
    left_join (iso_hurto, by="id") %>% 
    left_join (iso_lesiones, by="id") %>% 
    left_join (iso_robo, by="id") %>% 
    replace(is.na(.), 0)#reemplazamos todos los valores nulos por 0

# establecemos un criterio de ponderación de crimen de acuerdo a su violencia e indicidencia en la calidad de accesibilidad peatonal: 
ponderador_hurto <- 0.20
ponderador_lesiones <- 0.35
ponderador_robo <- 0.45

# juntamos los indicadores en un único índice ponderado
radios_CABA_crime <- radios_CABA_crime %>% 
    mutate(crime_index=hurto_index*ponderador_hurto+
               lesiones_index*ponderador_lesiones+
               robo_index*ponderador_robo) %>% 
    transmute(id, crime_index)


#inspección visual

ggplot()+
    geom_sf(data=radios_CABA_crime, aes(fill=crime_index), color="grey60")+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.1)+
    geom_sf(data=CABA_limite, fill=NA, size=1)+
    scale_fill_viridis_c(option = "magma", direction = -1)+
    labs(fill="Delito/hab radio censal")+
    theme_void()

st_write(radios_CABA_crime, "data/processed/GCABA/crime/radios_con_indice_crime_CABA.shp",  delete_layer = TRUE)
