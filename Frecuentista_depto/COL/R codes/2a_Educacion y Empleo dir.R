#########################################################
# Proyecto IPM                   #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
rm(list = ls())

# Loading required libraries ----------------------------------------------

library(patchwork)
library(trafo)
library(normtest)
library(nortest)
library(lme4)
library(tidyverse)
library(rstan)
library(rstanarm)
library(survey)
library(srvyr)
select <- dplyr::select
# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_ipm <-
  readRDS("Frecuentista_depto/COL/Data/encuesta_ids.rds")


diseno <- as_survey_design_(
  .data = encuesta_ipm,
  ids = ~ upm,
  weights = ~ fep,
  strata = ~ estrato,
  nest = TRUE
)

options(survey.lonely.psu = "adjust")
## Estimación directa por depto

diseno %<>% mutate(
  IPM = 0.1 * (
    ipm_Material +
      ipm_Saneamiento +
      ipm_Energia +
      ipm_Internet +
      ipm_Agua +
      ipm_Hacinamiento
  ) +
    0.2 * (ipm_educacion +   ipm_Empleo_Aseguramiento),
  IPM = ifelse(IPM >= 0.4, 1, 0)
)

Estimacion_dir <- diseno %>% group_by(depto) %>%
  summarise(
    nd = unweighted(n()),
    Educacion = survey_mean(ipm_educacion, vartype = c("cv")),
    Empleo = survey_mean(ipm_Empleo_Aseguramiento, vartype = c("cv")),
    IPM = survey_mean(IPM,vartype = c("cv")))

saveRDS(Estimacion_dir,
        file = "Frecuentista_depto/COL/data/Educacion_Empleo_IPM_dir.rds")
