library(sf)
library(tidyverse)

# cargamos los radios censales y seleccionamos solo los de CABA
radios_CABA <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = T) %>%
    st_transform(4326) %>% 
    filter(eph_aglome=="CABA")

# guardamos el dataset resultante.
st_write(radios_CABA, "data/raw/INDEC/radios_CABA.shp", delete_dsn = TRUE)

# Inspección visual
ggplot()+
    geom_sf(data=radios_CABA, fill="red")+
    theme_void()
# Reconocemos una falla en el dataset: Existen dos radios censales con valores nulos.

# Descargamos los datos actualizados de la Ciudad de Buenos Aires de la página de INDEC
# https://www.indec.gob.ar/indec/web/Institucional-Indec-Codgeo

radios_INDEC_CABA <- st_read("data/raw/INDEC/cabaxrdatos.shp") %>% 
    st_transform(4326)

# Inspección visual y diferencia entre datasets
ggplot()+
    geom_sf(data=radios_INDEC_CABA, fill="red", color="grey80", size=.1)+
    geom_sf(data=radios_CABA, fill="white", color="grey80", size=.1)+
    theme_minimal()


### Para este análisis vamos a reemplazar los radios por los más actualizados

