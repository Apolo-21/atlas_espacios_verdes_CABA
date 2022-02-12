library(osmdata)
library(sf)

###############################################################
# Descarga del límite geográfico de la Ciudad de Buenos Aires #
###############################################################

# Obtenemos en límite geográfico de la caba a partir de OSM.
caba_limite <- getbb("Ciudad Autónoma de Buenos Aires, Argentina", format_out = "sf_polygon")
caba_limite <- caba_limite$multipolygon    

# Asignamos el sistema de coordenadas EPSG:4326 - WGS 84.
caba_limite <- st_transform(caba_limite, crs = 4326)


# Lo guardamos:
st_write(caba_limite, "data/raw/OSM/limite_CABA.shp", delete_dsn = TRUE)
