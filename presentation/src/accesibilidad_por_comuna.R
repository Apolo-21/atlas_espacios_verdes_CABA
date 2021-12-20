library(sf)
library(tidyverse)
library(ggplot2)

sf::sf_use_s2(FALSE) #apagamos la geometría esférica

CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    st_difference()

comunas <- st_read("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/comunas/CABA_comunas.geojson") %>% 
    st_transform(4326) %>% 
    st_intersection(CABA_limite) %>% 
    mutate(COMUNAS=as.integer(COMUNAS))

EV <- st_read("data/processed/GCABA/EV/espacios-verdes-CABA-cualificados.shp")

radios_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp", stringsAsFactors = FALSE) %>%
    st_transform(4326) %>% 
    select(DEPTO, TOT_POB, PAIS0210_I) %>% 
    mutate(ID=PAIS0210_I, 
           COMUNAS= as.integer(DEPTO), #eliminamos el primer 0
           AREA=as.numeric(st_area(.)*0.0001)) %>%  #pasamos los m2 a ha
    replace(is.na(.), 0)

#IDs de radios y comunas
radios_CABA_aux <- radios_CABA %>% 
    as.data.frame() %>% 
    select(ID, COMUNAS, TOT_POB)

#poblacion por comuna
poblacion_por_comuna <- radios_CABA_aux %>% 
    group_by(COMUNAS) %>% 
    summarise(POB_COM=sum(TOT_POB))
sum(poblacion_por_comuna$POB_COM)

#accesbilidad caminando (ponderada):
accesibilidad <- st_read("data/processed/accesibilidad/accesibilidad_espacios_verdes_CABA.shp") %>% 
    st_transform(crs=4326) %>% 
    select(situacn, id) %>% 
    rename(ID=id) %>% 
    left_join(radios_CABA_aux, by="ID")


accesibilidad_por_comuna <- accesibilidad %>% 
    as.data.frame() %>% 
    dplyr:: filter(situacn=="con_acceso") %>% 
    group_by(COMUNAS) %>% 
    summarise(POB_ACC=sum(TOT_POB))
sum(accesibilidad_por_comuna$POB_ACC)

inaccesibilidad_por_comuna <- accesibilidad %>% 
    as.data.frame() %>% 
    dplyr:: filter(situacn=="sin_acceso") %>% 
    group_by(COMUNAS) %>% 
    summarise(POB_INACC=sum(TOT_POB))
sum(inaccesibilidad_por_comuna$POB_INACC)
sum(accesibilidad_por_comuna$POB_ACC)+sum(inaccesibilidad_por_comuna$POB_INACC)

comunas <- left_join(comunas, poblacion_por_comuna, by="COMUNAS")
comunas <- left_join(comunas, accesibilidad_por_comuna, by="COMUNAS")
comunas <- left_join(comunas, inaccesibilidad_por_comuna, by="COMUNAS")

comunas <- comunas %>% 
    mutate(PORC_ACC=POB_ACC/POB_COM*100,
           PORC_INACC=POB_INACC/POB_COM*100)
sum(comunas$PORC_INACC)

# inspeccion visual accesibilidad
ggplot()+
    geom_sf(data=comunas, aes(fill=PORC_ACC), show.legend = FALSE, size=1, color="black")+
    scale_fill_gradient(low="white", high = "#5a2163")+
    geom_sf_label(data=comunas, aes(label= paste("Comuna ", COMUNAS, "\n", round(PORC_ACC,2), "%")))+
    labs(title="Proporción de población con accesibilidad por comuna")+
    theme_void()

# inspeccion visual inaccesibilidad
ggplot()+
    geom_sf(data=comunas, aes(fill=PORC_INACC), show.legend = FALSE, size=1, color="black")+
    scale_fill_gradient(low="white", high = "#5a2163")+
    geom_sf_label(data=comunas, aes(label= paste("Comuna ", COMUNAS, "\n", round(PORC_INACC,2), "%")))+
    labs(title="Proporción de población sin accesibilidad por comuna")+
    theme_void()


# Barras
# lollipops
ggplot() +
    geom_segment(data = comunas, aes(x=reorder(COMUNAS, PORC_ACC), 
                                     xend=reorder(COMUNAS, PORC_ACC), 
                                     y=0, yend=PORC_ACC), color="grey70", size=1.5) +
    geom_segment(data = comunas, aes(x=reorder(COMUNAS, PORC_ACC), 
                                     xend=reorder(COMUNAS, PORC_ACC), 
                                     y=PORC_ACC, yend=100), color="red", size=1.5)+
    geom_point(data = comunas, aes(x=reorder(COMUNAS, PORC_ACC), 
                                   y=PORC_ACC), color="grey30", size=4) +
    theme_light() +
    theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    xlab("Comuna") +
    ylab("Accesibilidad por población")+
    ylim(c(00,100))


# Barras acumulativas
ggplot()+
    geom_bar(data = comunas, aes(x=reorder(COMUNAS, PORC_INACC), y=100), 
             stat="identity", fill="grey80", alpha=.5)+
    geom_bar(data = comunas, aes(x=reorder(COMUNAS, PORC_INACC), y=PORC_INACC), 
             stat="identity", fill="#5a2163")+
    
    geom_hline(yintercept=mean(comunas$PORC_INACC), linetype="dashed", color = "orangered", size=1)+
    
    geom_text(data=comunas, aes(x=reorder(COMUNAS, PORC_INACC), y=40,
                                label = paste(round(PORC_ACC,0),"%")), vjust = -10, color="grey50")+
    geom_text(data=comunas, aes(x=reorder(COMUNAS, PORC_INACC), y=PORC_INACC,
                                label = paste(round(PORC_INACC,0),"%")), vjust = -.5, fontface = "bold")+
    labs(title="Proporción de población sin accesibilidad por comuna",
         x="Comuna",
         y="Proporción (%)",
         fill="DSDA")+
    theme_minimal()


    




