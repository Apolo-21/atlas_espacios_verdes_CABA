library(tidyverse)
library(sf)


####################################################################
# Relevamiento de espacios verdes: Cerrados vs Abiertos - 1º Parte #
####################################################################

# El siguiente script busca establecer la condición de los espacios verdes públicos
# (EV) de la Ciudad de Buenos Aires (CABA) en lo que refiere a su "disponibilidad
# diaria". Para ello, se releva la presencia y/o ausencia de enrejado público que
# impida el acceso de los vecinos y vecinos a los mismos durante determinados momentos
# del día.

# Cargamos las bases de datos que utilizaremos en este script.
# 1. Espacios verdes de OSM.
areas_verdes_caba <- st_read("data/processed/osm/areas_verdes_urbanas_caba.shp") %>% 
    st_transform(st_crs(4326)) #Volvemos al sistema de coordenadas estándar.

# 2. Espacios verdes del portal de datos abiertos del Gobierno de la Ciudad de Buenos Aires (GCBA).
areas_verdes_gcba <- st_read("data/raw/GCABA/EV/espacio-verde-publico.shp") %>% 
    st_transform(st_crs(areas_verdes_caba))

# 3. Espacios verdes con información (incompleta) sobre cerramiento.
# Base de datos provista por el Ministerio de Espacio Público e Higiene Urbana (MEPHU) del GCBA tras consulta (1/10/2021).
rejas_gcba <- st_read("data/raw/GCABA/EV-cerrables/190710_Espacios_Verdes_200730_cerrables.shp") %>% 
    st_transform(st_crs(areas_verdes_caba))

summary(as.factor(rejas_gcba$Cierre)) 
# El dataset compartido por el GCBA con datos de cerramiento indica la existencia
# de 185 espacios verdes enrejados en el distrito.

# 4. Límite de la CABA.
caba_limite <- st_read("data/raw/osm/limite_CABA.shp") %>% 
    st_transform(st_crs(areas_verdes_caba))

# Inspección visual: OSM vs GCBA.
ggplot()+
    geom_sf(data = caba_limite, fill = "grey95", color = "black")+
    geom_sf(data = areas_verdes_gcba, aes(fill = "BA DATA"), color = NA, alpha = .5)+
    geom_sf(data = rejas_gcba, aes(fill = "MEPHU"), color = NA, alpha = .5)+
    geom_sf(data = areas_verdes_caba, aes(fill = "OSM"), color = NA, alpha = .5)+
    scale_fill_manual(values = c("OSM" = "blue", "BA DATA" = "red", "MEPHU" = "yellow"))+
    labs(title = "Espacios Verdes de la Ciudad de Buenos Aires",
         fill = "")+
    theme_minimal()

# Antes de continuar, es preciso reconocer que los tres datasets poseen información
# sobre un número diferente de espacios verdes. De hecho, podemos ver que los datasets
# de espacios públicos del GCBA contienen información sobre un mayor número de polígonos
# que el que contenía originalmente OSR para la CABA (1116. Ver script "02_processing/1._procesar_shapefile_de_espacios_verdes.R").
# Ello se debe, en parte, a que los primeros no fueron filtrados como el último a lo 
# largo de este trabajo. Pese a ello, este estudio decide utilizar como fuente a OSM
# a los fines de trabajar con una base de datos disponible en todas las localidades
# del país.

# Ahora, vamos a unir ambos datasets del GCBA con el de OSM con el fin de sumar la 
# información allí presente. Por este motivo, nos quedamos con los campos de interés
# de ambos datasets. 
areas_verdes_gcba <- areas_verdes_gcba %>% 
    rename(id = id_ev_pub) %>% 
    janitor::clean_names() %>% 
    select(c(2:4, 9, 16, 17, 22:24, 30, 31))

rejas_gcba <- rejas_gcba %>% 
    select(5:12, 18, 21) %>% 
    janitor::clean_names() %>% 
    rename(id = id_evuc)

# Inspección visual: BA DATA vs MEPHU.
ggplot()+
    geom_sf(data = caba_limite, fill = "grey95", color = "black")+
    geom_sf(data = areas_verdes_gcba, aes(fill = "BA DATA"), color = NA, alpha = .5)+
    geom_sf(data = rejas_gcba, aes(fill = "MEPHU"), color = NA, alpha = .5)+
    scale_fill_manual(values = c("MEPHU" = "yellow", "BA DATA" = "red"))+
    labs(title = "Espacios Verdes de la Ciudad de Buenos Aires",
         fill = "")+
    theme_minimal()

# Veamos si encontramos similitudes entre los IDs de ambos datasets.
n_random <- round(runif(1, 0,1529), 0)

areas_verdes_gcba %>% 
    filter(id == n_random) %>% 
    select(nombre_ev, ubicacion)

rejas_gcba %>% 
    filter(id == n_random) %>% 
    select(nombre_ev, ubicacion)

# A partir del siguiente método, podemos observar que los ID de los espacios verdes
# en ambos datasets no coinciden. Por este motivo, uniremos los datasets a través
# de una unión espacial.

# 1. BA DATA 
gcba_centroide <- areas_verdes_gcba %>%
    st_make_valid() %>% 
    select(-nombre_ev, -id, area) %>% 
    st_centroid() 

# Intersectamos con OSM.
areas_verdes_caba_2 <- st_join(areas_verdes_caba, gcba_centroide) %>% 
    st_difference()

# 2. MEPHU (Rejas)
rejas_centroide <- rejas_gcba %>%
    st_make_valid() %>% 
    select(6, 8, 10) %>% 
    st_centroid() 

# Intersectamos con OSM.
areas_verdes_caba_2 <- st_join(areas_verdes_caba_2, rejas_centroide) %>% 
    st_difference()

summary(as.factor(areas_verdes_caba_2$cierre))

# Ahora bien, ya tenemos un dataframe que reconoce: 
#   - la clasificiación del EV .
#   - la escala.
#   - el uso.
#   - si cuenta con un patio de juegos.
#   - si cuenta con postas aeróbicas.
#   - si cuenta con canil para perros.
#   - si cuenta con enrejado público.

# Ahora bien, a partir del cruce de datos, contamos con informacón sobre cerramiento
# para 489 de los 670 espacios verdes de la CABA. Ello quiere decir desconocemos la
# condición de casi un 30% (180) del total, por lo cual, es necesario introducir una
# nueva instancia de verificación.
# Con tal motivo, el siguiente trabajo realiza un relevamiento virtual a través del
# motor Google Street View (https://www.google.com.ar/maps/) de aquellos polígonos
# que no cuentan con la información pertinente.

# A continuación, exportamos el Excel para completar con los datos del relevamiento.
areas_verdes_caba_3 <- areas_verdes_caba_2 %>% 
    st_set_geometry(NULL) %>% 
    as.data.frame()

# Guardamos.
write_csv(areas_verdes_caba_3, "data/processed/GCABA/EV/Ev-a-completar.csv",
          na = "NA",
          append = F)
