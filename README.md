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

-   1_obtener_extracto_OSM_Argentina.R

-   2_obtener_cartografia_oficial.R

    3_obtener_datos_mortalidad_DEIS.R

-   4_obtener_ubicacion_barrios_populares.R

src/01b_preparation_CABA/

-   1_obtener_barrios_CABA.R
-   2_obtener_poligono_limite_CABA.R
-   3_obtener_calles_CABA.R
-   4_obtener_parking_CABA.R

src/02_processing/

-   1_procesar_shapefile_de_espacios_verdes.R

src/02_processing_CABA/

-   1_procesar_shapefile_de_espacios_verdes_CABA.R
-   2_completar_espacios_verdes_CABA.R
-   3_cualificar_espacios_verdes_CABA.R
-   4a_estimar_isocronas_a\_pie_10_minutos_CABA.R
-   4b_estimar_isocronas_en_bici_10_minutos_CABA.R
-   5a_estimar_seguridad_por_radio_censal_a\_pie_CABA.R
-   5b_estimar_seguridad_por_radio_censal_en_bici_CABA.R
-   6a_estimar_isocronas_a\_pie_CABA_ponderadas_por_inseguridad.R
-   6b_estimar_isocronas_en_bici_CABA_ponderadas_por_inseguridad.R
-   7a_medir_accesibilidad_a\_espacios_verdes_a\_pie_CABA_base_sin_ponderacion.R
-   7b_medir_accesibilidad_a\_espacios_verdes_a\_pie_CABA_ponderado.R
-   7b_medir_accesibilidad_a\_espacios_verdes_en_bici_CABA_ponderado.R
-   8a_extraer_metricas_de_accesibilidad_a\_espacios_verdes_CABA_base_sin_ponderacion.R
-   8b_extraer_metricas_de_accesibilidad_a\_espacios_verdes_a\_pie_CABA_ponderado.R
-   8c_extraer_metricas_de_accesibilidad_a\_espacios_verdes_en_bici_CABA_ponderado.R

src/03_modelling_CABA/

-   1_clusterizar_areas_deficitarias_CABA.R
-   2_indentificar_parcelas_potenciales_CABA.R
-   3a_determinar_margen_de_cobertura_cluster_16.R
-   3b_determinar_margen_de_cobertura_cluster_12.R
-   4a_indentificar_parcelas_potenciales_cluster_16_con_margen.R
-   4b_indentificar_parcelas_potenciales_cluster_12_con_margen.R
-   5a_asociar_infraestructura_a\_parcelas_cluster_16.R
-   5b_asociar_infraestructura_a\_parcelas_cluster_12.R
-   6a_modelo_de_localizacion_asignacion_parking_cluster_16.R
-   6b_modelo_de_localizacion_asignacion_parking_cluster_12.R
-   7a_modelo_de_localizacion_asignacion_lotes_y\_depositos_cluster_16.R
-   7b_modelo_de_localizacion_asignacion_lotes_y\_depositos_cluster_12.R
