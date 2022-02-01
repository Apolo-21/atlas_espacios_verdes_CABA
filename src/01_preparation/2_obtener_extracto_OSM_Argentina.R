####################################################
# Obtención de áreas naturales de Argentina de OSM #
####################################################

# Descargamos el extracto de OSM para la Argentina con sus áreas naturales desde geofrabrik.de (https://www.geofabrik.de/)
url <- "https://download.geofabrik.de/south-america/argentina-latest-free.shp.zip"

zipfile <- tempfile()

download.file(url, zipfile)

# Nos interesa la capa "gis_osm_landuse_a_free_1"
# La otra capa que suena pertinente, "gis_osm_natural_a_free_1" no tiene nada útil
# para nuestro caso. Sus categorias son:
# beach cave_entrance         cliff          peak        spring          tree       volcano 

layer_files <- c(c("gis_osm_landuse_a_free_1.cpg", "gis_osm_landuse_a_free_1.dbf", 
                   "gis_osm_landuse_a_free_1.prj", "gis_osm_landuse_a_free_1.shp", 
                   "gis_osm_landuse_a_free_1.shx", "README"))

unzip(zipfile, files = layer_files, exdir = "data/raw/OSM")

# Nos libramos del archivo temporal
unlink(zipfile)

# Nota: Geofabrik utiliza informacón proveniente de Open Street Map (https://www.openstreetmap.org/),
# un repositorio colaboraritvo para la construcción de mapas de uso libre. Ello implica
# que los archivos disponibles se encuentran en constante transformación. Por este
# motivo, y debido a las intancias mecánicas que comprende este trabajo, es posible
# que, al correr este script, algunos de los datos desarrollados en otras etapas de 
# este estudio queden desactualizados. 

# Aclaración: Los datos fueron descargados por última vez el 31 de enero de 2022.

