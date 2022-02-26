library(tidyverse)
library(sf)


################################################################
# Descarga de los radios censales de la Ciudad de Buenos Aires #
################################################################

# Cargamos los radios censales a nivel país y nos quedamos solo con aquellos pertenecientes
# a la Ciudad de Buenos Aires (CABA).
radios_caba <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = T) %>%
    st_transform(4326) %>% # Le asignamos el sistema de coordenadas estándar  WGS 84.
    filter(eph_aglome == "CABA")


# guardamos el dataset resultante.
st_write(radios_caba, "data/raw/INDEC/eph/radios_CABA.shp", delete_dsn = TRUE)

# Inspección visual.
ggplot()+
    geom_sf(data = radios_caba, fill = "red", color = "black")+
    theme_void()

# Reconocemos que el dataset de radios censales a nivel país cuenta con una falla;
# Existen dos radios censales con valores nulos en el barrio de Monserrat. Por este
# motivo, descargamos los datos específicos para la CABA de la página de INDEC.
# https://www.indec.gob.ar/indec/web/Institucional-Indec-Codgeo

radios_indec_caba <- st_read("data/raw/INDEC/radios_censales_caba/cabaxrdatos.shp") %>% 
    st_transform(4326) # Le asignamos el sistema de coordenadas estándar  WGS 84.

# Inspección visual: Diferencia entre datasets.
ggplot()+
    geom_sf(data = radios_indec_caba, fill = "red", color = "grey80", size = .1)+
    geom_sf(data = radios_caba, fill = "white", color = "grey80", size = .1)+
    theme_minimal()

# A partir del ejercicio anterior, podemos observar que el nuevo dataset contiene
# los radios censales que faltaban en el primer archivo que descargamos. Por este 
# motivo. de aquí en adelente, vamos a utilizar esta última versión descargada.

