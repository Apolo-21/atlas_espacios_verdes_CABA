###########################################################################
# Obtención de los espacios verdes de la Ciudad de Buenos Aires (BA DATA) #
###########################################################################

#Descargamos los espacios verdes públicos de la Ciudad Autónoma de Buenos Aires
# del portal de datos abiertos "BA Data" (https://data.buenosaires.gob.ar/)

url <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/secretaria-de-desarrollo-urbano/espacios-verdes/espacio-verde-publico.zip"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/GCABA/ev")

# Nos libramos del archivo temporal.
unlink(zipfile)
