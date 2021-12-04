library(osmdata)

############################################################
# Descargar límite geográfico de la Ciudad de Buenos Aires #
############################################################

# Obtenemos en límite geográfico de la CABA a partir de OSM
CABA_limite <- getbb("Ciudad Autónoma de Buenos Aires, Argentina", format_out = "sf_polygon")
CABA_limite <- CABA_limite$multipolygon    

# Asignamos el sistema de coordenadas EPSG:4326 - WGS 84:
CABA_limite <- st_transform(CABA_limite, crs = 4326)

# lo guardamos:
st_write(CABA_limite, "data/raw/OSM/limite_CABA.shp", delete_dsn = TRUE)
