#############################################
# Obtención de cartografia censal del INDEC #
#############################################

# NOTA: La página web que lista la cartogarfía vigente suele cambiar. 
# Es posible que al intentar correr este script, la misma haya cambiado.
# Al 31 de enero de 2022, es https://www.indec.gob.ar/indec/web/Nivel4-Tema-1-39-120

# Descargamos la cobertura geográfica de la Encuesta Permanente de Hogares (EPH)
# a nivel de radio censal (de acuerdo a la cartografía empleada en el Censo Nacional
# de Población, Hogares y Viviendas 2010).
url <- "https://www.indec.gob.ar/ftp/cuadros/territorio/radios_eph_json.zip"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/INDEC")

# Nos libramos del archivo temporal.
unlink(zipfile)
