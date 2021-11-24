# Atlas de Espacios Verdes e Índice de Accesibilidad para ciudades argentinas

Código, datos y reportes generados durante el relevamiento y cuantificación de accesibilidad de espacios verdes en ciudades argentinas. 

El código esta escrito en [R](https://www.r-project.org/).


## Reproducción de resultados

### Entorno de trabajo

Para recrear el entorno de desarrollo (es decir, instalar automáticamente todos los paquetes de R que el proyecto requiere) se emplean las funciones del paquete `renv`, que puede ser instalado desde [CRAN](https://cran.r-project.org/web/packages/renv/index.html):

1. Clonar o descargar este repositorio
2. Abrir el proyecto (archivo `atlas_espacios_verdes.Rproj`) desde RStudio. O, de no trabajar con RStudio, simplemente correr los siguientes comandos en una sesión de R lanzada desde la carpeta raíz del proyecto. 
3. Usar `renv::init()` para inicializar `renv` 
4. Al aparecer un menú con opciones, elegir "Restore the project from the lockfile."

### Procesamiento

Correr los scripts provistos, con ésta secuencia:  

src/01_preparation/
* 0_obtener_extracto_OSM_Argentina.R
* 1_obtener_cartografia_oficial.R
* 2_obtener_datos_mortalidad_DEIS.R
* 3_obtener_ubicacion_barrios_populares.R

src/02_processing/
* 0_procesar_shapefile_de_espacios_verdes.R
* 1_estimar_isocronas_a_pie.R
* 2_medir_accesibilidad_a_espacios_verdes.R
* 3_estimar_NSE.R
* 4_extraer_metricas_de_accesibilidad_a_espacios_verdes.R
* 5_generar_output_app_visualizacion.R

