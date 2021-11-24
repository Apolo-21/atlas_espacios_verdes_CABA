# Estimar isocronas a pie CABA

library(tidyverse) 
library(sf)

#cargamos las isocronas
isocronas_arg <- st_read("data/processed/isocronas/isocronas_10_min_a_pie_radios_urbanos.shp") %>% 
    st_transform(4326)%>% 
    mutate(id=as.integer(id))

## Cargamos radios censales urbanos de CABA
radios_CABA <- st_read("data/raw/INDEC/radios_CABA.shp", stringsAsFactors = FALSE) %>% 
    filter(tiporad == "U") %>% 
    st_transform(4326) %>% 
    mutate(as.integer(id))

ggplot()+
    geom_sf(data=radios_CABA, fill="black")

isocronas_CABA <- radios_CABA %>% 
    as.data.frame() %>% 
    select(id) %>%
    left_join(isocronas_arg, by="id")

st_write(isocronas_CABA, "data/processed/isocronas/isocronas_10_min_a_pie_CABA.shp")
