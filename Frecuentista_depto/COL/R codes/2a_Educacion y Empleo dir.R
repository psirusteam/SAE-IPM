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
library(survey)
library(srvyr)
# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
encuesta_ipm <- readRDS("Frecuentista_depto/COL/Data/encuesta_ipm.rds")

# Agregando encuesta ------------------------------------------------------

diseno <- as_survey_design_(.data = encuesta_ipm, 
                            ids = ~1,
                            weights = ~fep, 
                            nest = TRUE 
                              )

diseno %<>% mutate(IPM = 0.1 * (
  ipm_Material+
    ipm_Saneamiento+
    ipm_Energia+
    ipm_Internet+
    ipm_Agua+
    ipm_Hacinamiento) +
    0.2 * (ipm_educacion +   ipm_Empleo_Aseguramiento) )

Estimacion_dir <- diseno %>% group_by(depto) %>% 
  summarise(Educacion = survey_mean(ipm_educacion),
            Empleo = survey_mean(ipm_Empleo_Aseguramiento),
            IPM = survey_mean(IPM))

saveRDS(Estimacion_dir,
        file = "Frecuentista_depto/COL/data/Educacion_Empleo_IPM_dir.rds")          





