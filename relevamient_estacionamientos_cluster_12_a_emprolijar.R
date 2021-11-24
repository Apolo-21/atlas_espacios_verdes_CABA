
CABA_limite <- st_read("data/processed/osm/limite_CABA.shp") %>% 
    st_difference() 

proj <- st_crs(CABA_limite)

radios_cluster_12 <- st_read("data/processed/accesibilidad/radios_cluster_12.shp") %>% 
    st_transform(proj)

parking <- st_read("data/raw/Parking/parking_CABA.shp") %>% 
    st_transform(crs=proj)

parking <- parking %>% 
    st_intersection(radios_cluster_12) %>%
    select(osm_id) %>% 
    unique()

parking_guardar <- parking %>%
    st_set_geometry(NULL)

write.csv(parking_guardar, "parking_cluster_12.csv")

ggplot()+
    geom_sf(data = CABA_limite)+
    geom_sf(data = parking)+
    geom_sf(data = radios_cluster_12)


parking_relevado <- read.csv("parking_cluster_12_relevado.csv", sep = ";") %>% 
    unique() %>% 
    mutate(osm_id=as.character(osm_id))

parking <- left_join(parking, parking_relevado, by="osm_id")

st_write(parking, "parking_cluster_12_relevado.shp")




