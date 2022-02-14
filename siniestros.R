library(tidyverse)
library(sf)
sf_use_s2(FALSE) # Apagamos la geometría esférica.


#####################
# SINIESTROS VIALES #
#####################


################################################################################
# EL DATASET DE DELITOS TIENE UN SUBTIPO DE DELITO QUE ES "SINIESTRO VIAL", UNO
# DE LOS TIPOS DE DELITOS QUE MÁS LEVANTA LA LIT SOBRE CAMINABILIDAD. QUIZÁS LO
# PODEMOS INCLUIR O DAR MÁS PESO
################################################################################


# Carga de bases de datos.
# 1.límite de la Ciudad de Buenos Aires (CABA).
caba_limite <- st_read("data/raw/OSM/limite_CABA.shp") 

# 2. Comunas.
comunas <- st_read("data/raw/GCABA/Comunas/comunas.geojson") 

# 3. Isocronas (a pie).
isocronas_caba <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_CABA.shp") %>% 
    st_transform(crs = 4326) %>% 
    select(id)

# 4. Radios censales (CABA).
radios <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(crs = 4326) %>% 
    janitor::clean_names() %>% 
    rename(id = pais0210_i)

# 5. Delitos de la Ciudad de Buenos Aires (2020).
delito <- read.csv("data/raw/GCABA/delito/delitos_2020.csv")

# Inspeccionemos las estadísticas de delitos registrados según tipo de crimen.
delito %>%
    group_by(tipo, subtipo) %>% 
    summarise(cantidad = n()) %>% 
    print(Inf)


# Ahora, Vamos a transformar nuestro dataset de delitos en uno espacial. Para ello,
# primero, vamos a corregir aquellas coordenas que se enecuentran mal registradas.
delito <- delito %>% 
    filter(subtipo == "Siniestro Vial") %>% 
    drop_na(latitud, longitud) %>% # Quitamos los registros no geolocalizados.
    mutate(latitud = case_when(
        (latitud < -40) ~ latitud/1000,
        (latitud >= -40 ~ latitud)),
        longitud = case_when(
            (longitud < -60) ~ longitud/1000,
            (longitud >= -60 ~ longitud)))


# Luego, lo convertimos en un objeto espacial.
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

## Inseguridad del entorno caminable: Radios censales --------------------------


# La siguiente etapa consiste en transpolar los valores del mapa de calor de delitos
# de la CABA a los radios censales de la misma.

# Para comenzar, unimos espacialmente las iscronas base (de 10 minutos a pie desde
# el centroide de cada radio censal) con los delitos, a fin de observar la incidencia
# de estos últimos sobre el entorno caminable.
iso_delito <- st_join(delito, isocronas_caba) # ESTO ME GENERA CIERTAS DUDAS. LA INTERSECCÓN ENTRE DELITOS E ISOCRONAS DA CASI 4 MILLNES DE OBJETOS???

# Agrupemos las iscronas por ID.
iso_delito_id <- iso_delito %>% 
    as.data.frame() %>% 
    select(id, tipo) %>% 
    group_by(id, tipo) %>%
    summarise(cant_delito_id = n())

# Luego, cargamos los radios censales e intersectamos con los delitos por isocrona
# con el objetivo de normalizar los valores por población.
radios_df <- radios %>% 
    as.data.frame() %>% 
    select(id, tot_pob) %>% 
    replace(is.na(.), 0)

iso_delito_id <- iso_delito_id %>% 
    left_join(radios_df, by ="id")

# Primero, vamos a crear un índice de homicidios por entorno caminable. 
iso_homicidio <- iso_delito_id %>% 
    filter(tipo =="Homicidio") %>% 
    mutate(homicidio_index = cant_delito_id/tot_pob) %>% # Incidencia del radio censal sobre isocrona.
    transmute(id, homicidio_index) 

# Remplazamos los valores infinitos producto de dividir por 0, por 0.
iso_homicidio <- do.call(data.frame, lapply(iso_homicidio, function(x) replace(x, is.infinite(x), 0)))


# Luego, vamos a construir un índice de lesiones.
iso_lesiones <- iso_delito_id %>% 
    filter(tipo =="Lesiones") %>% 
    mutate(lesiones_index = cant_delito_id/tot_pob,
           lesiones_index = replace_na(lesiones_index, 0)) %>% # Incidencia del radio censal sobre isocrona. 
    transmute(id, lesiones_index)

# Remplazamos los valores infinitos producto de dividir por 0, por 0.
iso_lesiones <- do.call(data.frame,lapply(iso_lesiones, function(x) replace(x, is.infinite(x), 0)))

# Una vez generados los índices de criminalidad por isocrona, vamos a proceder a 
# unir la información a los radios censales correspondientes.
radios_caba_crimen <- radios %>% 
    left_join(iso_homicidio, by = "id") %>% 
    left_join(iso_lesiones, by = "id") %>% 
    replace(is.na(.), 0) # Reemplazamos todos los valores nulos por 0.

# Establecemos un criterio de ponderación del crimen de acuerdo a su violencia e    # ESTA PARTE ESTA FLOJA DE PELPAS.
# indicidencia en la calidad de accesibilidad peatonal: 
ponderador_homicidio <- 0.55
ponderador_lesiones <- 0.45

# Juntamos los indicadores de inseguridad en un único índice ponderado.
radios_caba_crimen <- radios_caba_crimen %>% 
    mutate(crime_index = homicidio_index*ponderador_homicidio + lesiones_index*ponderador_lesiones) %>% 
    transmute(id, crime_index)

#inspección visual.
ggplot()+
    geom_sf(data = radios_caba_crimen, aes(fill = crime_index), color = "grey60")+
    geom_sf(data = comunas, fill = NA, size = .1, color = "black", alpha = .1)+
    geom_sf(data = caba_limite, fill = NA, size = 1)+
    scale_fill_viridis_c(option = "magma", direction = -1)+
    labs(fill = "Siniestros/hab radio censal")+
    theme_void()
