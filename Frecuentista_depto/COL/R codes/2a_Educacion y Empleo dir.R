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


diseno %>% group_by(depto) %>% 
  summarise(Educacion = survey_mean(ipm_educacion, vartype = c("se", "cv")),
            Empleo = survey_mean(ipm_Empleo_Aseguramiento, vartype = c("se", "cv"))) %>% 
  openxlsx::write.xlsx(
    file = "Frecuentista_depto/COL/Output/Educacion_y_Empleo_dir.xlsx",
    overwrite = TRUE)




