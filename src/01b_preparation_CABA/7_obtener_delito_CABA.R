########################################################################
# Descarga de las estadísticas de delitos de la Ciudad de Buenos Aires #
########################################################################

# Descargamos los crímenes registrados en la Ciudad Autónoma de Buenos Aires durante
# el 2020 del portal de datos abiertos "BA Data" (https://data.buenosaires.gob.ar/).

# ACLARACIóN: Al 31 de enero de 2022, los últimos datos de delito para la Ciudad
# disponibles en el portal de datos abiertos del gobierno distrital corresponden
# al año 2020.

delitos <- read.csv("https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-justicia-y-seguridad/delitos/delitos_2020.csv",
                    stringsAsFactors = T,
                    encoding = "UTF-8")


# Guardamos
write.csv(delitos, "data/raw/GCABA/delito/delitos_2020.csv", append = F, row.names = F)
