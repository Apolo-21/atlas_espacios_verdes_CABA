library(sf)
library(osmdata)

################################################################################
#Descargar limite geografico de la Ciudad
################################################################################

#Obtenemos en límite geográfico de CABA
CABA_limite <- getbb("Ciudad Autónoma de Buenos Aires, Argentina", format_out = "sf_polygon")
CABA_limite <- CABA_limite$multipolygon    

#Asignamos el sistema de coordenadas EPSG:4326 - WGS 84:
CABA_limite <- st_transform(CABA_limite, crs = 4326)

#lo guardamos:
st_write(CABA_limite, "data/raw/OSM/limite_CABA.shp", delete_dsn = TRUE)
