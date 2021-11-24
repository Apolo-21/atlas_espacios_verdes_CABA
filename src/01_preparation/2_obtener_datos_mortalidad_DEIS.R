library(readxl)
library(tidyverse)
library(sf)
    
# Los datos de mortalidad por departamento fueron obtenidos de 
# http://www.deis.msal.gov.ar/index.php/estadisticas-vitales/
# Tienen un sistema muy molesto para descargar los datos, que requiere cliquear 
# en formularios javascript. Para no perder tiempo en automatizar un scraping que probablemente 
# deje de funcionar en cuanto hagan el siguiente cambio al sitio, hemos descargado a mano 
# los datos de mortalidad en CABA y PBA para los cinco años del periodo 2014 - 2018.
#   
# Vamos a generar un .csv con la mortalidad en departamentos del AMBA. En la práctica, 
# las comunas de la CABA y los partidos del conurbano

exceles <- dir("data/raw/DEIS", full.names = TRUE, recursive = TRUE)

leer_con_anio <- function(filepath) {
    anio <- str_sub(filepath, 15, 18)
    planilla <- read_excel(filepath)
    cbind(anio, planilla)
}

mortalidad_2014_2018 <-  map_df(exceles, leer_con_anio) %>% 
    filter(!is.na(DepRes), DepRes != "999") %>% 
    rename(codprov = ProvRes, coddepto = DepRes)




denominacion_partidos_AMBA_EPH <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    st_set_geometry(NULL) %>% 
    filter(tiporad == "U", eph_aglome %in% c("CABA", "Partidos del GBA")) %>% 
    select(eph_aglome, codprov, coddepto, localidade) %>% 
    distinct() %>% 
    # Retiramos el identificador censal que precede al nombre
    mutate(localidade = str_replace(localidade, "\\(.*\\) ", "")) %>% 
    # Le devolvemos a cada comuna de la CABA su identificación, que se pierde en el paso anterior
    mutate(localidade = ifelse(localidade == "Ciudad Autónoma de Buenos Aires",
                               paste("CABA Comuna", str_remove(coddepto, "0")),
                               localidade))
    
mortalidad_AMBA <- denominacion_partidos_AMBA_EPH %>% 
    left_join(mortalidad_2014_2018) %>% 
    select(-`Jurisdicciones Departamentos`) %>% 
    # definimos a cada comuna de la CABA como una "localidad" distinta
    mutate(localidade = ifelse(localidade == "Ciudad Autónoma de Buenos Aires",
                               paste("CABA Comuna", str_remove_all(coddepto, "0")),
                               localidade))

write_csv(mortalidad_AMBA, "data/processed/DEIS/mortalidad_AMBA_2014_2018.csv")
