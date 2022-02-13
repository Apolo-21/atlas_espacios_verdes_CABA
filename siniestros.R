library(tidyverse)
library(sf)

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
    janitor::clean_names()

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
    filter(subtipo == "Siniestro Vial" | subtipo == "Siniestro vial") %>% 
    # Se excluyen del análisis los homicidios, siendo estos poco frecuentes comparativamente.
    
    
    # UN 40% DE LOS HOMICIDIOS SON POR SINIESTROS VIALES. DEJAR AFUERA ESTA CATEGORÍA TIENE UN PESO
    # SOBRE NUESTRA HIPÓTESIS SOBRE CAMINABILIDAD DEL ENTORNO
    
    
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
