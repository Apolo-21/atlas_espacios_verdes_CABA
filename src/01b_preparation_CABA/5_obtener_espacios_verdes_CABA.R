#####################################
# Obtener espacios verdes (BA DATA) #
#####################################

#Descargamos los espacios verdes públicos de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos "BA Data" (https://data.buenosaires.gob.ar/)

url <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/espacios-verdes/espacio-verde-publico.zip"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/GCABA/EV")

# Nos libramos del archivo temporal.
unlink(zipfile)
