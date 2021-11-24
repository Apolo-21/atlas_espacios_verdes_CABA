########################################################################
# Crear un Índice de Nivel Socioeconómico para cada aglomerado urbano  #
########################################################################

library(psych)
library(tidyverse)


radios_ciudades <- st_read("data/raw/INDEC/radios_eph.json", stringsAsFactors = FALSE) %>% 
    filter(tiporad == "U") %>% 
    st_transform(4326) %>% 
    # Corregimos un error de tipeo en la data de origen ("Constit*i*ución")           
    mutate(eph_aglome = ifelse(eph_aglome == "San Nicolas - Villa Constitiución",
                               "San Nicolas - Villa Constitución",
                               eph_aglome)) %>% 
    # Retiramos el identificador censal que precede al nombre
    mutate(localidade = str_replace(localidade, "\\(.*\\) ", "")) %>% 
    # Le devolvemos a cada comuna de la CABA su identificación, que se pierde en el paso anterior
    mutate(localidade = ifelse(localidade == "Ciudad Autónoma de Buenos Aires",
                               paste("CABA Comuna", str_remove(coddepto, "0")),
                               localidade))


# Cargamos datos del Censo 2010
#
# Usaremos como variables para estimar NSE:
# • Nivel educacional del principal sostén del hogar (PSH)
# • Nivel ocupacional del PSH
# • Posesiones materiales del hogar
# https://repositorio.cepal.org/bitstream/handle/11362/6032/1/S028552_es.pdf

# PERSONA.Computadora._.Sí por "Tenencia de bienes materiales"
# PERSONA.Nivel.educativo.cursa.o.cursó._.Universitario por "Nivel de educación"
# PERSONA.Condición.de.actividad._.Desocupado por "Categoría ocupacional (desempleo)"

radios_vars_nse <- read_csv("data/raw/INDEC/censo_PHV_2010.csv") %>% 
    #filter(PERSONAS > 30) %>% # si quisieramos descartar radios (cuasi)despoblados
    transmute(RADIO,
              PERSONAS,
              nivel_universitario_o_superior = PERSONA.Nivel.educativo.cursa.o.cursó._.Universitario +
                  PERSONA.Nivel.educativo.cursa.o.cursó._.Post.universitario,
              posesion_computadora = PERSONA.Computadora._.Sí,
              desocupacion = PERSONA.Condición.de.actividad._.Desocupado /
                  (PERSONA.Condición.de.actividad._.Desocupado + PERSONA.Condición.de.actividad._.Ocupado))

# Armamos un identificador de radio comparable entre datasets y cruzamos

radios_ciudades <- radios_ciudades %>% 
    mutate(RADIO = paste0(codprov, coddepto, frac2010, radio2010)) %>% 
    left_join(radios_vars_nse)


# Generamos Indice NSE:
# analisis de factores
# extraccion de loadings
# y estimacion de indice como suma del valor de cada variable por su loading
# 
#    "One of the simplest ways to estimate factor scores for each individual involves summing raw scores
#     corresponding to all items loading on a factor (Comrey & Lee, 1992)"
# Understanding and Using Factor Scores: Considerations for the Applied Researcher
# https://scholarworks.umass.edu/cgi/viewcontent.cgi?article=1226&context=pare

extraer_indice_NSE <- function(nivel_universitario_o_superior,
                               posesion_computadora,
                               desocupacion,
                               deciles = TRUE) {
    
    matriz <- cbind(nivel_universitario_o_superior, 
                    posesion_computadora, 
                    desocupacion)
    
    # Para exploracion
    #vss(matriz, fm = "mle", title = "Variables: bienes materiales, educación, ocupación")
    
    NSE2010.fa <- fa(matriz, nfactors = 1, rotate = "varimax", fm = "mle")

    # Para exploracion
    #print(NSE2010.fa,cut=0,digits=3)
    #plot(NSE2010.fa, title ="Variables 2010 - factor loadings (I)")
    #fa.diagram(NSE2010.fa, main = "Variables 2010 - factor loadings (II)")
    
    # Extraemos loadings
    NSE_2010_loadings <- NSE2010.fa$loadings[,1]
    
    # Estimamos indice NSE como score de sus componentes.
    # usando un "non-refined method",
    # I. Normalizar las variables
    # 2. Para cada fila, multiplicar la variable por su loding
    # 3. Sumar los valores por fila para obtener un numero final 
    indice <- matriz %>%
        scale() %>%
        # la division por la suma de loadings  -la parte de "/ sum(NSE_2010_loadings)"-
        # hace que el scoring quede de 0 a 1 en los casos prolijamente aditivo - 
        # por ejemplo, cuando las variables van de 0 a 1
        # y todos los loadings tienen el mismo signo.
        # Cuando eso no ocurre, no hay daño... es solo una transformación lineal
        {. %*% NSE_2010_loadings / sum(NSE_2010_loadings)} 
    
    # Por default entrega deciles
    if(deciles) { ntile(indice, n = 10) } else indice

    
}

# Este ejercicio muestra que el non-refined method
# tiene correlacion casi perfecta con los metodos "refinados"  


# CABA <- radios_ciudades %>% filter(eph_aglome == "CABA")


# Primero el metodo simple

# non_refined_peasant_method_scores <- extraer_indice_NSE(CABA$nivel_universitario_o_superior,
#                                                 CABA$posesion_computadora,
#                                                 CABA$desocupacion)

# Ahora scoring con el método de Thurstone, el default en la funcion factor.scores():
#    factor.scores uses four different ways of estimate factor scores. 
#    In all cases, the factor score estimates are based upon the data matrix, X, times a weighting matrix, 
#    W, which weights the observed variables.
# 
#    For polytomous or dichotmous data, factor scores can be estimated using Item Response Theory techniques
#    (e.g., using link{irt.fa} and then link{scoreIrt}. Such scores are still just factor score estimates,
#    for the IRT model is a latent variable model equivalent to factor analysis.
#    
#    method="Thurstone" finds the regression based weights: 
#    W = R^-1 F
#    where R is the correlation matrix and F is the factor loading matrix."     

                                                                                     
# very_refined_Thurstone_method <- factor.scores(matriz,
#                                               fa(matriz, nfactors = 1, rotate = "varimax", fm = "mle"))

# A ver si se parecen:
# 
# cor(non_refined_peasant_method_scores, 
#     very_refined_Thurstone_method$scores, 
#     use = "pairwise")

# DA UNA CORRELACION DE 0.9875418 
# VENGA NOMAS EL METODO NO REFINADO PERO PARSIMONIOSO... Y CUASI EQUIVALENTE

radios_deciles_NSE <- radios_ciudades %>% 
    group_by(eph_aglome) %>% 
    mutate(decil_NSE = extraer_indice_NSE(nivel_universitario_o_superior, 
                                          posesion_computadora,
                                          desocupacion)) %>% 
    # Solo nos quedamos con ID de radio, poblacion y NSE
    select(RADIO, PERSONAS, decil_NSE) %>% 
    # No necesitamos los poligonos
    st_set_geometry(NULL)

# Guardamos los resultados

write_csv(radios_deciles_NSE, "data/processed/NSE/radios_urbanos_decil_NSE_por_aglomerado_EPH.csv")
