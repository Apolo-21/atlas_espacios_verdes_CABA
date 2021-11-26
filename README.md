# Esto podría ser una plaza: modelo de localización - asignación para la refuncionalización verde de infraestructuras vacantes

#### [Concurso Datos Abiertos y Ciudades Verdes](https://www.fundacionbyb.org/datos-abiertos-y-ciudades-verdes)

*Fundación Bunge y Born*

### **Equipo de investigación**

-   [Santiago Soubié](https://www.linkedin.com/in/santiago-soubie-55783b135/)

-   [Luis Emilio Tisocco](https://www.linkedin.com/in/luis-emilio-tisocco-88a83915b/)

-   [Ángeles Scetta](https://www.linkedin.com/in/mar%C3%ADa-de-los-%C3%A1ngeles-scetta-b7b82a80/)

Este trabajo se desprende como una continuación del Atlas de Espacios Verdes de la Fundación Bunge y Born.Contiene datos generados durante la redefinición de la accesibilidad a espacios verdes para la Ciudad Autónoma de Buenos Aires y un modelo de localización-asignación para cubrir las áreas sin accesibilidad de la manera más eficiente, incorporando infraestructura potencialmente reconvertible.

El código esta escrito en [R](https://www.r-project.org/).

### Reproducción de resultados

#### Entorno de trabajo

Para recrear el entorno de desarrollo (es decir, instalar automáticamente todos los paquetes de R que el proyecto requiere) se emplean las funciones del paquete `renv`, que puede ser instalado desde [CRAN](https://cran.r-project.org/web/packages/renv/index.html):

1.  Clonar o descargar este repositorio
2.  Abrir el proyecto (archivo `atlas_espacios_verdes.Rproj`) desde RStudio. O, de no trabajar con RStudio, simplemente correr los siguientes comandos en una sesión de R lanzada desde la carpeta raíz del proyecto.
3.  Usar `renv::init()` para inicializar `renv`
4.  Al aparecer un menú con opciones, elegir "Restore the project from the lockfile."

#### Procesamiento (ACTUALIZAR ESTA PARTE)

Correr los scripts provistos, con ésta secuencia:

src/01_preparation/

-   0_obtener_extracto_OSM_Argentina.R
-   1_obtener_cartografia_oficial.R
-   2_obtener_datos_mortalidad_DEIS.R
-   3_obtener_ubicacion_barrios_populares.R

src/02_processing/

-   0_procesar_shapefile_de_espacios_verdes.R
-   1_estimar_isocronas_a\_pie.R
-   2_medir_accesibilidad_a\_espacios_verdes.R
-   3_estimar_NSE.R
-   4_extraer_metricas_de_accesibilidad_a\_espacios_verdes.R
-   5_generar_output_app_visualizacion.R
