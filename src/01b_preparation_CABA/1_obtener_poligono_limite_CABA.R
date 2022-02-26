library(osmdata)
library(sf)

################################################################
# Obtención del límite geográfico de la Ciudad de Buenos Aires #
################################################################

# Descargamos en límite geográfico de la Ciudad de Buenos Aires a partir de OSM.
caba_limite <- getbb("Ciudad Autónoma de Buenos Aires, Argentina", format_out = "sf_polygon")
caba_limite <- caba_limite$multipolygon    

# Asignamos el sistema de coordenadas EPSG:4326 - WGS 84.
caba_limite <- caba_limite %>% 
    st_transform(crs = 4326) %>% 
    st_difference()


# Lo guardamos.
st_write(caba_limite, "data/raw/OSM/limite_CABA.shp", delete_dsn = TRUE)
