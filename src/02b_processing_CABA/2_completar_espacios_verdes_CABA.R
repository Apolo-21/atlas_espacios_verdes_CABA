library(tidyverse)
library(sf)
library(leaflet)

#########################################################################################
#Completar toda la informacion de espacios verdes + relevamiento
#########################################################################################


## Carga de bases
# espacios verdes de OSM
areas_verdes_CABA <- st_read("data/processed/osm/areas_verdes_CABA.shp") %>% 
    st_transform(st_crs(4326)) #Le cambié el sistema de coordenadas.

# cargamos los espacios verdes del portal de datos del GCBA 
areas_verdes_GCBA <- st_read("data/raw/GCABA/EV/espacio-verde-publico.shp") %>% 
    st_transform(st_crs(areas_verdes_CABA))
                 
# cargamos la información de espacios verdes enrejados, provista por el GCABA pero no completa 
rejas_GCBA <- st_read("data/raw/GCABA/EV-cerrables/190710_Espacios_Verdes_200730_cerrables.shp") %>% 
    st_transform(st_crs(areas_verdes_CABA))

summary(as.factor(rejas_GCBA$cierre))

# inspección visual OSM vs GCBA
ggplot()+
    geom_sf(data=areas_verdes_GCBA, aes(fill="OSM"), color=NA, alpha=.5)+
    geom_sf(data=areas_verdes_CABA, aes(fill="GCBA"), color=NA, alpha=.5)+
    geom_sf(data=rejas_GCBA, aes(fill="Rejas"), color=NA, alpha=.5)+
    scale_fill_manual(values = c("OSM" = "blue", "GCBA"="red", "Rejas"="yellow"))+
    labs(fill="")+
    theme_minimal()

#_______________________________________________________________________________

# Primero, vamos a unir el dataset de EV del GCBA con el de rejas del GCBA.
# Nos quedamos con los campos de interés de ambos datasets. 

areas_verdes_GCBA <- areas_verdes_GCBA %>% 
    rename(id=id_ev_pub) %>% 
    clean_names() %>% 
    select(c(2:4, 9, 16, 17, 22:24, 30, 31))
 
rejas_GCBA <- rejas_GCBA %>% 
    select(5:12, 18, 21) %>% 
    clean_names() %>% 
    rename(id=id_evuc)

# Veamos si encontramos similutdes entre los IDs de ambos datasets.
n_random <- round(runif(1, 0,1529), 0)

areas_verdes_GCBA %>% 
    filter(id==n_random) %>% 
    select(nombre_ev, ubicacion)

rejas_GCBA %>% 
    filter(id==n_random) %>% 
    select(nombre_ev, ubicacion)

# A partir del siguiente método, podemos observar que los ID de los espacios verdes en ambos datasets no coinciden.
# Por este motivo, uniremos ambos datasets a través de una unión espacial.

areas_verdes_GCBA <- areas_verdes_GCBA %>% 
    st_make_valid()

rejas_GCBA <- rejas_GCBA %>% 
    st_make_valid()

rejas_centroide <- rejas_GCBA %>%
    select(6, 8, 10) %>% 
    st_centroid() # Si todo saliese bien, debiese de ser posible interceptar ambas geometrías sin necesidad de hacer los centroides con la F(x) st_intersection(areas_verdes_GCBA, rejas_GCBA). Yo lo intente, pero mi pc es tán mala que tardaba mil años

areas_verdes_GCBA_2 <- st_join(areas_verdes_GCBA, rejas_centroide) 
areas_verdes_GCBA_2 <- unique(areas_verdes_GCBA_2)

summary(as.factor(areas_verdes_GCBA_2$cierre))

# Inspección visual
ggplot()+
    geom_sf(data=areas_verdes_GCBA_2, fill="red", color=NA)+
    geom_sf(data=areas_verdes_CABA, fill="grey50", color=NA)

# Podemos ver que el dataset de espacios públicos del GCBA contiene información
# sobre un mayor número de polígonos que el que contiene OSR.
# Ello se debe en parte a que el dataset del GCBA no fue filtrado por los mismos
# parámetros que aplicamos al de OSM a lo largo de este trabajo.
# Sin embargo, a los fines de utilizar una base de datos que pueda ser disponible
# en todas las localidades del país, optamos por usar OSM como base.

# Ahora bien, ya tenemos un dataframe que reconoce: 
#   - la clasificiación del EV 
#   - la escala
#   - el uso
#   - si tiene patio de juegos
#   - si tiene equipamiento aeróbico
#   - si tiene canil para perros
#   - si es cerrable

# calculamos los centroides de este último dataframe para unirlo espacialmente al de OSM
EV_GCBA_centroid <- areas_verdes_GCBA_2 %>% 
    select(-nombre_ev, -id, area) %>%
    st_centroid() 
    
# intersectamos con nuestros espacios verdes
areas_verdes_CABA_2 <- st_join(areas_verdes_CABA, EV_GCBA_centroid) %>% 
    st_difference()

# exportamos el Excel para completar con los datos del relevamiento
areas_verdes_CABA_3 <- areas_verdes_CABA_2 %>% 
    st_set_geometry(NULL) %>% 
    as.data.frame()

summary(as.factor(areas_verdes_CABA_3$cierre))

write_csv(areas_verdes_CABA_3, "data/processed/GCABA/EV/Ev-a-complete.csv",
          na = "NA",
          append = T)

# cargamos el set de datos que relevamos, para volver a agregarle los atributos geográficos

areas_verdes_CABA_4 <- read.csv("data/processed/GCABA/EV/Ev-completo.csv", stringsAsFactors = TRUE, 
                                encoding = "UTF-8")

areas_verdes_CABA_2 <- areas_verdes_CABA_2 %>% 
    select(osm_id, geometry) %>% 
    mutate(osm_id=as.numeric(osm_id))

areas_verdes_CABA_2 <- areas_verdes_CABA_2 %>% 
    left_join(areas_verdes_CABA_4, by="osm_id")

summary(areas_verdes_CABA_2$cierre)

#guardamos los datos procesados
st_write(areas_verdes_CABA_2, "data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp", append = F)

#_______________________________________________________________________________
# inspección visual con las nuevas categorías de EV #Hay que modificar nombres.

leaflet() %>% 
    addPolygons(data= areas_verdes_CABA_2,
                popup = ~paste("<br><b>NOMBRE:</b>", name,
                              "<br><b>ID OSM:</b>", osm_id,
                              "<br><b>CLUSTER ID:</b>", cluster_id,
                              "<br><b>ENREJADO:</b>", cierre,
                              "<br><b>ESCALA:</b>", escala,
                              "<br><b>JUEGOS:</b>", patio_de_j,
                              "<br><b>CANIL:</b>", canil,
                              "<br><b>GYM:</b>", posta_aero,
                              "<br><b>CLASIFICACIÓN:</b>", clasificac)) %>% 
    addTiles()


ggplot()+
    #geom_sf(data=areas_verdes_CABA, fill="blue", color=NA)+
    geom_sf(data=areas_verdes_CABA_2, aes(fill=clasificac), color=NA)+
    geom_sf(data=areas_verdes_CABA_2 %>% filter(cierre=="Abierto"), color="violet", fill=NA)+
    geom_sf(data=areas_verdes_CABA_2 %>% filter(cierre=="Cerrable"), color="blue", fill=NA)+
    geom_sf(data=areas_verdes_CABA_2 %>% filter(is.na(cierre)), fill="red")+
    theme_minimal()
