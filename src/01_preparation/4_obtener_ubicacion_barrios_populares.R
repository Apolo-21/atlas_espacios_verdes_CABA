# Datos del Registro Nacional de Barrios Populares (2017)

url <- "https://infra.datos.gob.ar/catalog/otros/dataset/3/distribution/3.2/download/barrios-populares.zip"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/RENABAP")

# Nos libramos del archivo temporal
unlink(zipfile)
