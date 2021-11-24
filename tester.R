library(sf)
library(leaflet)
library(tidyverse)
library(readxl)

osm <- read_sf("data/raw/OSM")

indec_carto <- read_sf("data/raw/INDEC")

summary(indec_carto)

ggplot(indec_carto)+
    geom_sf()

indec_carto <- indec_carto %>% 
    st_transform(4326)

leaflet(indec_carto) %>%
    addPolygons() %>% 
    addTiles()

IGN <- read_sf("data/raw/IGN")

radios_INDEC_arg <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = T) %>%
    st_transform(4326) %>% 
    filter(eph_aglome=="CABA")

leaflet(radios_INDEC_arg) %>%
    addPolygons() %>% 
    addTiles()

summary(radios_INDEC_arg)

ggplot(filter(radios_INDEC_arg, tiporad!="U"))+
    geom_sf(aes(fill=tiporad))

leaflet(filter(radios_INDEC_arg, tiporad=="")) %>% 
    addTiles() %>% 
    addPolygons()

#Estamos seguros que esos radios tienen un valor?

radios_INDEC_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(4326)
    
summary(radios_INDEC_CABA)

ggplot()+
    geom_sf(data=radios_INDEC_CABA, fill="red", color="grey80", size=.1)+
    geom_sf(data=radios_INDEC_arg, fill="white", color="grey80", size=.1)+
    theme_minimal()

# 2.2

acces <- read.csv("data/processed/accesibilidad/espacios_verdes_mas_de_media_ha_a_10_m_caminando.csv")


# 2b. 1

library(leaflet)

areas_verdes_CABA <- areas_verdes_CABA %>% 
    st_transform(crs = 4326)

leaflet(areas_verdes_CABA) %>% 
    addTiles() %>% 
    addPolygons()

#2b.5

summary(as.factor(rejas_GCBA$cierre)) #Ojo acá, el dataset de espacios rejas tiene todos los espacios verdes identificados como cerrados o abiertos. Mienteras que el resultado final de OSM tenemos 400 NA's. Más que la dif entre el de BA Data y el de rejas.

#que pasa si intersectamos los centroides del dataset de rejas directamente con el de OSM?


leaflet(areas_verdes_CABA) %>% 
    addTiles() %>% 
    addPolygons()

# En el de OSM, varias oportunidades no se agrupan los Clusters - Ejemplo: Parque Centenario y Eva Perón

#Me encontré con geometrías inválidas en los datasets del GCBA?

areas_verdes_x_rejas <- st_join(areas_verdes_CABA, rejas_centroide) %>% 
    st_difference()

summary(as.factor(areas_verdes_x_rejas$cierre)) #Tenemos casi 500 parques identificados con abierto o cerrados
summary(as.factor(areas_verdes_GCBA_2$cierre))
summary(as.factor(areas_verdes_CABA_2$cierre))

rejas_GCBA2 <- rejas_GCBA %>% 
    mutate(valido = st_is_valid(geometry)) %>% 
    filter(valido=="FALSE")

leaflet(rejas_GCBA2) %>% 
    addPolygons() %>% 
    addTiles()

rejas2 <- rejas_GCBA2 %>% 
    st_make_valid()

leaflet(rejas2) %>% 
    addPolygons() %>% 
    addTiles()

# Dataset: BA DATA

areas_verdes_GCABA2 <- areas_verdes_GCABA %>% 
    mutate(valido = st_is_valid(geometry)) %>% 
    filter(valido=="FALSE")

leaflet(areas_verdes_GCABA2) %>% 
    addPolygons() %>% 
    addTiles()

verdes2 <- areas_verdes_GCABA2 %>% 
    st_make_valid()

leaflet(verdes2) %>% 
    addPolygons() %>% 
    addTiles()

casos <- areas_verdes_GCABA_2 %>% 
    group_by(id) %>%
    summarise(n=n()) %>% 
    arrange(desc(n)) %>% 
    head(22)

leaflet(casos) %>% 
    addPolygons() %>% 
    addTiles()

# Alternativa a 2b. 5

verdes_sfc <- st_geometry(verdes)

verdes_sfc_centroides <- st_centroid(verdes_sfc)

verdes_scale <- (verdes_sfc - verdes_sfc_centroides)*0.5 + verdes_sfc_centroides

verdes <- verdes %>% 
    st_set_geometry(verdes_scale)

leaflet(verdes) %>% 
    addPolygons() %>% 
    addTiles()

verdes2 <- verdes %>% 
    st_set_geometry(if_else(verdes$fencing=="Cerrable", verdes_scale, verdes_sfc))

# 2.11 Visualizacion

barrios <- read_sf("data/raw/GCABA/Barrios/barrios.shp")

ggplot()+
    geom_sf(data=radios_CABA, fill="grey66", color="grey96")+
    geom_sf(data=espacios_verdes, fill="seagreen")+
    geom_sf(data=filter(base_combinada, situacion=="sin_acceso"), aes(fill="Radios censales ponderados\na más de 10 min a bici hasta\nla plaza o parque más próximo"))+
    geom_sf(data = barrios, color="black", fill=NA)+
    scale_fill_manual(values = c("Radios censales ponderados\na más de 10 min a bici hasta\nla plaza o parque más próximo"="red"))+
    labs(fill="")+
    theme_void()

# 2b. 5 Visualización

ggplot()+
    geom_sf(data=radios_INDEC_arg, fill="grey66", color="grey96")+
    geom_sf(data = verdes, aes(fill="Diferencia"))+
    geom_sf(data = verdes_cualificados, fill="seagreen")+
    labs(fill="")+
    theme_void()

ggplot()+
    geom_sf(data=radios_INDEC_arg, fill="grey66", color="grey96")+
    geom_sf(data = verdes, aes(fill=fencing))+
    labs(fill="Espacio")+
    theme_void()
                
library(readxl)

#Comparar EV to complete vs EV completed

nuevo <- read.csv("data/processed/GCABA/EV/Ev-a-complete.csv",
                  stringsAsFactors = T,
                  encoding = "UTF-8")


completo <- read.csv("data/processed/GCABA/EV/Ev-completo.csv", stringsAsFactors = TRUE, 
                                encoding = "UTF-8")


union <- full_join(nuevo, completo, by="osm_id")

summary(as.factor(nuevo$cierre))

summary(as.factor(completo$cierre))


