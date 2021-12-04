# Descargamos el extracto de OSM para la Argentina con sus áreas naturales desde geofrabrik.de

url <- "https://download.geofabrik.de/south-america/argentina-latest-free.shp.zip"

zipfile <- tempfile()

download.file(url, zipfile)


# Nos interesa la capa "gis_osm_landuse_a_free_1"
# La otra capa que suena pertinente, "gis_osm_natural_a_free_1" no tiene nada util para nuestro caso; 
# sus categorias son:
# beach cave_entrance         cliff          peak        spring          tree       volcano 

layer_files <- c(c("gis_osm_landuse_a_free_1.cpg", "gis_osm_landuse_a_free_1.dbf", 
                   "gis_osm_landuse_a_free_1.prj", "gis_osm_landuse_a_free_1.shp", 
                   "gis_osm_landuse_a_free_1.shx", "README"))

unzip(zipfile, files = layer_files, exdir = "data/raw/OSM")

# Nos libramos del archivo temporal
unlink(zipfile)
