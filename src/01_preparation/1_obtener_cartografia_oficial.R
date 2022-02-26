################################################################
# Obtención de la cartografia censal de la República Argentina #
################################################################

# Descargamos los radios censales pertenecientes a la Encuesta Permanente de Hogares
# (EPH) desde la página del INDEC.

# NOTA: La página web que lista la cartogarfía vigente suele cambiar. Por este motivo,
# es posible que, al intentar correr este script, la misma haya variado.
# Al 24 de febrero de 2022, es https://www.indec.gob.ar/indec/web/Nivel4-Tema-1-39-120

url <- "https://www.indec.gob.ar/ftp/cuadros/territorio/radios_eph_json.zip"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/INDEC")

# Nos libramos del archivo temporal.
unlink(zipfile)
