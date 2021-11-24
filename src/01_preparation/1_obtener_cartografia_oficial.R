# Obtener cartografia censal de INDEC

# NOTA: La página web que lista la cartogarfía vigente suele cambiar. 
# Es posible que al intentar correr este script haya cambiado.
# Al 5 de febrero de 2020, es https://www.indec.gob.ar/indec/web/Nivel4-Tema-1-39-120


url <- "https://www.indec.gob.ar/ftp/cuadros/territorio/radios_eph_json.zip"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/INDEC")

# Nos libramos del archivo temporal
unlink(zipfile)



# Obtener cartografía de IGN

url <- "http://ramsac.ign.gob.ar/operaciones_sig/shp_from_geoserver/download.php?f=Z2VvanNvbjo6ZGVwYXJ0YW1lbnRvLnppcA%3D%3D"

zipfile <- tempfile()

download.file(url, zipfile)

unzip(zipfile, exdir = "data/raw/IGN")

# Nos libramos del archivo temporal
unlink(zipfile)


