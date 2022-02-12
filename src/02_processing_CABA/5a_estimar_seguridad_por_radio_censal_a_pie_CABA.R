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
# 1.límite de la Ciudad de Buenos Aires (CABA).
caba_limite <- st_read("data/raw/OSM/limite_CABA.shp") 

# 2. Comunas.
comunas <- st_read("data/raw/GCABA/Comunas/comunas.geojson") 

# 3. Isocronas (a pie).
isocronas_caba <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
    st_transform(crs = 4326) %>% 
    select(id)

# 4. Radios censales CABA.
radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(crs = 4326) %>% 
    janitor::clean_names()

# 5. Delitos (2020).
delito <- read.csv("data/raw/GCABA/delito/delitos_2020.csv")



################################################################################
# EL DATASET DE DELITOS TIENE UN SUBTIPO DE DELITO QUE ES "SINIESTRO VIAL", UNO
# DE LOS TIPOS DE DELITOS QUE MÁS LEVANTA LA LIT SOBRE CAMINABILIDAD. QUIZÁS LO
# PODEMOS INCLUIR O DAR MÁS PESO
################################################################################



# Inspeccionemos las estadísticas de delitos registrados según tipo de crímen.
delito %>%
    group_by(tipo) %>% 
    summarise(cantidad = n())

# Ahora, Vamos a transformar nuestro dataset de delitos en uno espacial. Para ello,
# primero, vamos a corregir aquellas coordenas que se enecuentran mal registradas.
delito <- delito %>% 
    filter(tipo != "Homicidio") %>%
    # Se excluyen del análisis los homicidios, siendo estos poco frecuentes comparativamente.
    drop_na(latitud, longitud) %>% # Quitamos los registros no geolocalizados.
    mutate(latitud = case_when(
                (latitud < -40) ~ latitud/1000,
                (latitud >= -40 ~ latitud)),
           longitud = case_when(
                 (longitud < -60) ~ longitud/1000,
                 (longitud >= -60 ~ longitud)))
    
# Luego, lo trasnformamos en un objeto espacial.
delito <- delito %>% 
    st_as_sf(coords = c("longitud", "latitud"),
             crs = 4326) %>% 
    st_intersection(caba_limite) %>% 
    mutate(lat = unlist(map(geometry, 2)),
           long = unlist(map(geometry, 1)))


# Inspección visual: Incidencia del delito.
ggplot() +
    geom_sf(data = caba_limite, fill = "white", size = 1)+
    stat_density_2d(data = delito, aes(x = long, y = lat, fill = stat(level)), color = "grey20", size = 0.8, linetype = "dashed", geom = "polygon")+
    geom_sf(data = comunas, fill = NA, size = .1, color = "black", alpha = .3)+
    scale_fill_viridis_c(direction = -1, option = "magma")+
    labs(fill="Delitos\n2020")+
    theme_void()

# El siguiente paso consiste en transpolar la información del mapa de calor a los
# radios censales.

# Unimos espacialmente las iscronas base (10 mins) con los delitos, para ver su0
# incidencia en el entorno caminable.
iso_delito <- st_join(delito, isocronas_caba)

iso_delito_id <- iso_delito %>% 
    as.data.frame() %>% 
    select(id, tipo) %>% 
    group_by(id, tipo) %>%
    summarise(cant_delito_id = n())

# Luego, cargamos los radios censales, para normalizar los valores obtenidos por población.
radios_df <- radios %>% 
    as.data.frame() %>% 
    select(pais0210_i, tot_pob) %>% 
    rename(id = pais0210_i) %>% 
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
radios_caba_crime <- radios %>% 
    left_join (iso_hurto, by="id") %>% 
    left_join (iso_lesiones, by="id") %>% 
    left_join (iso_robo, by="id") %>% 
    replace(is.na(.), 0)#reemplazamos todos los valores nulos por 0

# establecemos un criterio de ponderación de crimen de acuerdo a su violencia e indicidencia en la calidad de accesibilidad peatonal: 
ponderador_hurto <- 0.20
ponderador_lesiones <- 0.35
ponderador_robo <- 0.45

# juntamos los indicadores en un único índice ponderado
radios_caba_crime <- radios_caba_crime %>% 
    mutate(crime_index=hurto_index*ponderador_hurto+
               lesiones_index*ponderador_lesiones+
               robo_index*ponderador_robo) %>% 
    transmute(id, crime_index)


#inspección visual

ggplot()+
    geom_sf(data=radios_caba_crime, aes(fill=crime_index), color="grey60")+
    geom_sf(data=comunas, fill=NA, size=.1, color="black", alpha=.1)+
    geom_sf(data=caba_limite, fill=NA, size=1)+
    scale_fill_viridis_c(option = "magma", direction = -1)+
    labs(fill="Delito/hab radio censal")+
    theme_void()

st_write(radios_caba_crime, "data/processed/GCABA/crime/radios_con_indice_crime_CABA.shp",  delete_layer = TRUE)
