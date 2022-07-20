#########################################################
# Proyecto IPM                   #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list =ls())

# Loading required libraries ----------------------------------------------

library(patchwork)
library(trafo)
library(normtest)
library(nortest)
library(lme4)
library(tidyverse)
library(rstan)
library(rstanarm)

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_ipm <- readRDS("Frecuentista_mpio/COL/Data/encuesta_ipm.rds")
tasa_desocupados <- readRDS("Frecuentista_mpio/COL/Data/tasa_desocupacion.rds")

# Agregando encuesta ------------------------------------------------------
statelevel_predictors_df <- tasa_desocupados
byAgrega <- c("mpio", "area", "sexo", "edad",
              "ipm_Material",
              "ipm_Hacinamiento",
              "ipm_Agua",
              "ipm_Saneamiento",
              "ipm_Energia",
              "ipm_Internet" )


encuesta_df_agg <- encuesta_ipm %>% 
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            No_empleo = sum(ipm_Empleo_Aseguramiento),
            empleo = n - No_empleo, .groups = "drop") 


encuesta_df_agg <- inner_join(encuesta_df_agg,
                              statelevel_predictors_df,
                              by = c("mpio"))


#--- Fit in stan_glmer ---#

fit <- glmer(
  cbind(No_empleo, empleo) ~  (1 | mpio) +
    edad +
    area +
    # anoest +
    # etnia +
    # mpio:area +
    # mpio:etnia +
    # mpio:sexo +
    # mpio:edad +
    # mpio:anoest +
    # area:etnia +
    # area:sexo +
    # area:edad +
    # area:anoest +
    # etnia:sexo +
    # etnia:edad +
    # etnia:anoest +
    # sexo:edad +
    # sexo:anoest +
    # edad:anoest +
    ipm_Material+
    ipm_Hacinamiento+
    ipm_Agua+
    ipm_Saneamiento +
    ipm_Energia + 
    ipm_Internet +
    sexo  + tasa_desocupacion +
    F182013_stable_lights + 
    X2016_crops.coverfraction +
    X2016_urban.coverfraction  ,
  family = binomial(link = "logit"),
  data = encuesta_df_agg
)



saveRDS(fit, file = "Frecuentista_mpio/COL/Data/fit_freq_empleo.rds")



