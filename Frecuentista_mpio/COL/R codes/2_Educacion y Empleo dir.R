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
encuesta_ipm <- readRDS("Frecuentista_mpio/COL/Data/encuesta_ipm.rds")
upms <- readRDS("Frecuentista_mpio/COL/Data/upm_dpto_2018.rds")
encuesta <- readRDS("Frecuentista_mpio/COL/Data/encuesta2018.rds")
# # Agregando upms y estratos ------------------------------------------------------
# encuesta %<>% left_join(upms,
#                          by = c("directorio" = "DIRECTORIO",
#                                 "secuencia_p" = "SECUENCIA_P",
#                                 "orden"))



diseno <- as_survey_design_(.data = encuesta_ipm, 
                            ids = ~1,
                            weights = ~fep, 
                            nest = TRUE 
                              )


Estimacion_dir <- diseno %>% group_by(mpio) %>% 
  summarise(Educacion = survey_mean(ipm_educacion),
            Empleo = survey_mean(ipm_Empleo_Aseguramiento))

saveRDS(Estimacion_dir,
file = "Frecuentista_mpio/COL/Output/Educacion_y_Empleo_dir.rds")          




