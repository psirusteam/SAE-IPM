################################################################################
## Proyecto del IPM = HxA 
## Andrés Gutiérrez y Stalyn Guerrero 
################################################################################

################################################################################
## Librerias 
################################################################################
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
encuesta_ipm <- saveRDS("Modelo_bayes_HxA/COL/Data/encuesta_COL.rds") 
statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds") 

byAgrega <- c("depto", "mpio", "area", "sexo", "etnia", 
              "anoest", "edad", "condact3" )

## Organizando las base de datos por dimension del ipm

names_ipm <- grep(pattern = "ipm", names(encuesta_ipm),value = TRUE)

encuesta_df <- map(setNames(names_ipm,names_ipm),
                   function(y){
                     encuesta_ipm$temp <- encuesta_ipm[[y]]
                     encuesta_ipm %>% 
                       group_by_at(all_of(byAgrega)) %>%
                       summarise(n = n(),
                                 yno = sum(temp),
                                 ysi = n - yno, .groups = "drop") %>% 
                       inner_join(statelevel_predictors_df,
                                  by = c("depto","mpio"))
                   })

### Definiendo el modelo multinivel.

# Para cada dimensión que compone el IPM se ajusta el siguiente modelo
# mostrado en el script. 

plan(multisession, workers = 4)

fit_agua <- stan_glmer(
    cbind(yno, ysi) ~ (1 | mpio) +
      (1 | depto) +
      edad +
      area +
      anoest +
      etnia +
      sexo + 
      tasa_desocupacion ,
    family = binomial(link = "logit"),
    data = encuesta_df$ipm_Agua,
    cores = 7,
    chains = 4,
    iter = 300
  )

  saveRDS(fit_agua, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_agua.rds")

fit_educacion <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_educacion,
  cores = 7,
  chains = 4,
  iter = 300
)


saveRDS(fit_educacion, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_educacion.rds")

fit_empleo <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Empleo_Aseguramiento,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_empleo, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_empleo.rds")


fit_energia <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Energia,
  cores = 7,
  chains = 4,
  iter = 300
)


saveRDS(fit_energia, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_Energia.rds")

fit_hacinamiento <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Hacinamiento,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_hacinamiento, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_Hacinamiento.rds")

fit_internet <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Internet,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_internet, file = "Data/fit_bayes_internet.rds")

fit_material <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Material,
  cores = 7,
  chains = 4,
  iter = 300
)

saveRDS(fit_material, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_material.rds")

fit_saneamiento <- stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = encuesta_df$ipm_Saneamiento,
  cores = 7,
  chains = 4,
  iter = 300
)


saveRDS(fit_saneamiento, file = "Modelo_bayes_HxA/COL/Data/fit_bayes_saneamiento.rds")



