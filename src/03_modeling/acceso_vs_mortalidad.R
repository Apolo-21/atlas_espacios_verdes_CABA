library(tidyverse)

accesibilidad <- read_csv("data/processed/metricas/accesibilidad_espacios_verdes_localidades.csv") 

mortalidad_AMBA_2014_2018 <- read_csv("data/processed/DEIS/mortalidad_AMBA_2014_2018.csv") %>% 
    mutate(tasa_m_infantil = (`Menores de 1`/`Nacidos Vivos`) * 1000,
           # Esto esta mal, necesitariamos la cantidad de partos. Pero para explorar lo hacemos
           tasa_m_maternal = (`Muertes Maternas`/`Nacidos Vivos`) * 1000)

# tomamos el promedio de los 5 años

mortalidad_AMBA_promedio_2014_2018 <- mortalidad_AMBA_2014_2018 %>% 
    group_by(eph_aglome, localidade) %>% 
    summarise(tasa_m_infantil_promedio = mean(tasa_m_infantil),
              tasa_m_maternal_promedio = mean(tasa_m_maternal))



####


get_effect_m_infantil <- function(dframe) {
    
    decil = last(dframe$decil_NSE)
    
    dframe %>% 
        {lm(tasa_m_infantil_promedio ~ tasa_acceso , data = .)} %>% 
        broom::tidy() %>% 
        mutate(p.value = round(p.value, 3)) %>% 
        {cbind(decil, .)} %>% 
        filter(term != "(Intercept)")
}


plot_model <- function(dframe) {
    
    decil = last(dframe$decil_NSE)
    
    p <- ggplot(dframe, aes(x = tasa_acceso, y = tasa_m_infantil_promedio)) +
        geom_point() +
        geom_text(aes(label = localidade)) +
        geom_smooth(method = "lm") +
        labs(title = paste(decil))
}


# Encontramos que una mayor tasa de acceso a espacios verdes esta correlacionada con una menor
# mortandad infantil... para todos los estratos de la población, excepto el más bajo!

accesibilidad %>% 
    filter(eph_aglome %in% c("CABA", "Partidos del GBA")) %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA_promedio_2014_2018) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map_df(get_effect_m_infantil)


# Comparado contra una regresión de todos los deciles juntos, y el decil como predictor
accesibilidad %>% 
    filter(eph_aglome %in% c("CABA", "Partidos del GBA")) %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA_promedio_2014_2018) %>% 
    {lm(data = ., tasa_m_infantil_promedio ~ tasa_acceso + total_ha_accesibles + m2_accesibles_per_capita + decil_NSE)} %>% 
    summary

# Scatterplots

accesibilidad %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA_promedio_2014_2018) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map(plot_model)
    

### Mortalidad maternal
# Tambien encontramos que una mayor tasa de acceso a esapacios verdes esta correlacionada con una menor
# tasa de (mortandad materna / nacimientos)... excepto para los estratos más bajos de la población! 
# todos menos deciles NSE 1 a 3


get_effect_m_maternal <- function(dframe) {
    
    decil = last(dframe$decil_NSE)
    
    dframe %>% 
        {lm(tasa_m_maternal_promedio ~ tasa_acceso , data = .)} %>% 
        broom::tidy() %>% 
        mutate(p.value = round(p.value, 3)) %>% 
        {cbind(decil, .)} %>% 
        filter(term != "(Intercept)")
}


accesibilidad %>% 
    filter(!is.na(decil_NSE)) %>% 
    right_join(mortalidad_AMBA_promedio_2014_2018) %>% 
    group_by(decil_NSE) %>% 
    group_split() %>% 
    map_df(get_effect_m_maternal)

###
###

accesibilidad_aglos <- read_csv("data/processed/metricas/accesibilidad_espacios_verdes_aglomerados.csv")

ggplot(accesibilidad_aglos) +
    geom_col(aes(x = decil_NSE, y = m2_accesibles_per_capita, fill = factor(decil_NSE))) +
    facet_wrap(~eph_aglome, scales = "free_y") +
    labs(title = "Disponibilidad de espacios verdes (m2 per cápita) según nivel socioeconómico de la población",
         x = NULL, y = "espcio verde per cápita (m2)", fill = "nivel socioeconómico (decil)",
         subtitle = "Aglomerados urbanos de la Argentina") +
    scale_fill_viridis_d(guide = guide_legend(direction = "horizontal", 
                                               title.position = "top", nrow = 1, label.position = "bottom", 
                                               keyheight = 0.5, keywidth = 0.75)) +
    theme_minimal() +
    theme(text = element_text(family = "Lato", color = "gray15"),
          legend.position = "bottom")

ggsave(filename = "reports/plots/m2_vs_NSE.png", width = 13, height = 8)
