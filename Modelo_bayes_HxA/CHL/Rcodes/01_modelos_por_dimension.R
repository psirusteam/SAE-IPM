################################################################################
## Proyecto del IPM = HxA 
## Andrés Gutiérrez y Stalyn Guerrero 
################################################################################

################################################################################
## Librerias 
################################################################################
rm(list = ls())
library(tidyverse)
library(rstantools)
library(rstan)
library(posterior)
library(patchwork)
library(lme4)
library(rstanarm)
library(magrittr)
library(furrr)

################################################################################
# Lectura de base de datos
################################################################################
encuesta_ipm <- readRDS("Modelo_bayes_HxA/CHL/Data/encuesta2017_nbi.Rds") 
statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/CHL/Data/statelevel_predictors_df_dam2.rds") %>% 
  mutate_at(.vars = c("luces_nocturnas",
                      "cubrimiento_cultivo",
                      "cubrimiento_urbano",
                      "modificacion_humana",
                      "accesibilidad_hospitales",
                      "accesibilidad_hosp_caminado"),
            function(x) as.numeric(scale(x)))

byAgrega <- c("dam", "dam2", "area", "sexo", "etnia", 
              "anoest", "edad" )

## Organizando las base de datos por dimension del ipm

names_ipm <- grep(pattern = "nbi", names(encuesta_ipm),value = TRUE)

encuesta_df <- map(setNames(names_ipm,names_ipm),
                   function(y){
                     encuesta_ipm$temp <- encuesta_ipm[[y]]
                     encuesta_ipm %>% 
                       group_by_at(all_of(byAgrega)) %>%
                       summarise(n = n(),
                                 yno = sum(temp),
                                 ysi = n - yno, .groups = "drop") %>% 
                       inner_join(statelevel_predictors_df,
                                  by = c("dam2"))
                   })

### Definiendo el modelo multinivel.

# Para cada dimensión que compone el IPM se ajusta el siguiente modelo
# mostrado en el script. 

plan(multisession, workers = 4)

fit_agua <- stan_glmer(
    cbind(yno, ysi) ~ (1 | dam2) +
      (1 | dam) +
      (1|edad) +
      area +
      (1|anoest) +
      (1|etnia) +
      sexo + 
      tasa_desocupacion +
      luces_nocturnas +
      modificacion_humana,
    family = binomial(link = "logit"),
    data = encuesta_df$nbi_agua,
    cores = 7,
    chains = 4,
    iter = 300
  )

saveRDS(fit_agua, file = "Modelo_bayes_HxA/COL/Data/fits_bayes_nbi_agua.rds")

fit_educacion <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_educacion,
  cores = 7,
  chains = 4,
  iter = 300
)


saveRDS(fit_educacion, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_educacion.rds")

fit_empleo <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Empleo_Aseguramiento,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_empleo, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_empleo.rds")


fit_energia <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Energia,
  cores = 7,
  chains = 4,
  iter = 300
)


saveRDS(fit_energia, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_Energia.rds")

fit_hacinamiento <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Hacinamiento,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_hacinamiento, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_Hacinamiento.rds")

fit_internet <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Internet,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_internet, file = "Data/fit_bayes_internet.rds")

fit_material <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Material,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_material, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_material.rds")

fit_saneamiento <- stan_glmer(
  cbind(yno, ysi) ~ (1 | dam2) +
    (1 | dam) +
    (1|edad) +
    area +
    (1|anoest) +
    (1|etnia) +
    sexo + 
    tasa_desocupacion +
    luces_nocturnas +
    modificacion_humana,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Saneamiento,
  cores = 7,
  chains = 4,
  iter = 300
)


saveRDS(fit_saneamiento, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_saneamiento.rds")



