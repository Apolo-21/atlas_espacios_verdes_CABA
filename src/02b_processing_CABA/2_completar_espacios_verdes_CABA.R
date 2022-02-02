library(tidyverse)
library(sf)
library(leaflet)

#########################################################
# Relevamiento de espacios verdes: Cerrados vs Abiertos #
#########################################################


# El siguiente script busca establecer la condición de los espacios verdes públicos (EV)
# de la Ciudad de Buenos Aires en lo que refiere a su "disponibilidad diaria". Para 
# ello, se releva la presencia y/o ausencia de enrejado público que impida el acceso
# a los mismos durante determinados momentos del día.


# Cargamos las bases de datos que utilizaremos en este script.
# 1. Espacios verdes de OSM.
areas_verdes_caba <- st_read("data/processed/osm/areas_verdes_CABA.shp") %>% 
    st_transform(st_crs(4326)) #Volvemos al sistema de coordenadas estándar.

# Espacios verdes del portal de datos del Gobierno de la Ciudad de Buenos Aires (GCBA).
areas_verdes_gcba <- st_read("data/raw/GCABA/EV/espacio-verde-publico.shp") %>% 
    st_transform(st_crs(areas_verdes_caba))
                 
# Espacios verdes con información (incompleta) sobre cerramiento.
# Información provista por el Ministerio de Espacio Público e Higiene Urbana del GCBA tras consulta.
rejas_gcba <- st_read("data/raw/GCABA/EV-cerrables/190710_Espacios_Verdes_200730_cerrables.shp") %>% 
    st_transform(st_crs(areas_verdes_caba))

summary(as.factor(rejas_gcba$Cierre)) 
# El dataset compartido por el GCBA con datos de cerramiento indica la existencia
# de 185 espacios verdes enrejados en el distrito

#-------------------------------------------------------------------------------
# EN EL INFORME PODRÏAMOS APLICAR EL MISMO FILTRO QUE LE APLICAMOS A LOS DE OSM
# A LOS DEL GCBA
#-------------------------------------------------------------------------------


# Inspección visual: OSM vs GCBA.
ggplot()+
    geom_sf(data=areas_verdes_gcba, aes(fill="CABA"), color=NA, alpha=.5)+
    geom_sf(data=rejas_gcba, aes(fill="Rejas"), color=NA, alpha=.5)+
    geom_sf(data=areas_verdes_caba, aes(fill="OSM"), color=NA, alpha=.5)+
    scale_fill_manual(values = c("OSM" = "blue", "GCBA"="red", "Rejas"="yellow"))+
    labs(fill="")+
    theme_minimal()

# Antes de continuar, es preciso reconocer que los tres datasets poseen información
# sobre un número diferente de espacios verdes. En este trabajo, optamos por utilizar
# el repositorio de OSM como base para nuestra investigación. Ello obedece a la intención
# de respetar la estructura original del Atlas de Espacios verdes de la Fundación Bunge
# & Born, así como a la aspiración de crear una metodología que sea luego replicable
# en otras ciudades del país.

# Ahora, vamos a unir el dataset de EV del GCBA con el de rejas del GCBA.
# Nos quedamos con los campos de interés de ambos datasets. 
areas_verdes_gcba <- areas_verdes_gcba %>% 
    rename(id=id_ev_pub) %>% 
    janitor::clean_names() %>% 
    select(c(2:4, 9, 16, 17, 22:24, 30, 31))
 
rejas_gcba <- rejas_gcba %>% 
    select(5:12, 18, 21) %>% 
    janitor::clean_names() %>% 
    rename(id=id_evuc)

# Veamos si encontramos similutdes entre los IDs de ambos datasets.
n_random <- round(runif(1, 0,1529), 0)

areas_verdes_gcba %>% 
    filter(id==n_random) %>% 
    select(nombre_ev, ubicacion)

rejas_gcba %>% 
    filter(id==n_random) %>% 
    select(nombre_ev, ubicacion)

# A partir del siguiente método, podemos observar que los ID de los espacios verdes
# en ambos datasets no coinciden. Por este motivo, uniremos ambos datasets a través
# de una unión espacial.


#-------------------------------------------------------------------------------
# Y SI EN VEZ DE UNIR LOS DOS DE LA CIUDAD Y DESPUËS CON EL DE OSM, LOS UNIMOS
# INDIVIDUALMENTE CON EL OSM POR SEPARADO?
#-------------------------------------------------------------------------------

areas_verdes_GCBA <- areas_verdes_GCBA %>% 
    st_make_valid()

rejas_GCBA <- rejas_GCBA %>% 
    st_make_valid()

rejas_centroide <- rejas_GCBA %>%
    select(6, 8, 10) %>% 
    st_centroid() 

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

# Calculamos los centroides de este último dataframe para unirlo espacialmente al de OSM.
EV_GCBA_centroid <- areas_verdes_GCBA_2 %>% 
    select(-nombre_ev, -id, area) %>%
    st_centroid() 
    
# Intersectamos con nuestros espacios verdes
areas_verdes_CABA_2 <- st_join(areas_verdes_CABA, EV_GCBA_centroid) %>% 
    st_difference()

# Exportamos el Excel para completar con los datos del relevamiento
areas_verdes_CABA_3 <- areas_verdes_CABA_2 %>% 
    st_set_geometry(NULL) %>% 
    as.data.frame()

summary(as.factor(areas_verdes_CABA_3$cierre))

write_csv(areas_verdes_CABA_3, "data/processed/GCABA/EV/Ev-a-complete.csv",
          na = "NA",
          append = T)

# Cargamos el set de datos que relevamos, para volver a agregarle los atributos geográficos.
# IMPORTANTE: El relevamiento fue realizado entre el 8 y 12 de octubre de 2021. A la fecha
# actual, puede que el dataset de espacios relevados se encuentre desactualizado y/o que 
# diverja de los resultados arrojados por OSM, en la medida que esta última es una base de  
# datos pública y colaborativa en constante transformación.

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
# inspección visual con las nuevas categorías de EV.

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
