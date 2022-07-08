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
library(bayesplot)
select <- dplyr::select
source("Frecuentista_depto/0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
estimacion_dir <- readRDS(
  file = "Frecuentista_depto/COL/data/Educacion_Empleo_IPM_dir.rds") %>% 
  select(!ends_with("_se"))

ipm_educacion <- readRDS(
  file = "Frecuentista_depto/COL/data/ipm_educacion.rds") 
ipm_empleo <- readRDS(
  file = "Frecuentista_depto/COL/data/ipm_empleo.rds") 
IPM <- readRDS(
  file = "Frecuentista_depto/COL/data/ipm_MC.rds") 

estimacion_dir <- full_join(estimacion_dir,ipm_educacion)
estimacion_dir <- full_join(estimacion_dir,ipm_empleo)
estimacion_dir <- full_join(estimacion_dir,IPM)
par(mfrow = c(1,3))

plot(estimacion_dir$Educacion, estimacion_dir$ipm_educacion_estimado_MC)
abline(b=1, a = 0, col = 2)


plot(estimacion_dir$Empleo, estimacion_dir$ipm_empleo_estimado_MC)
abline(b=1, a = 0, col = 2)

plot(estimacion_dir$IPM, estimacion_dir$ipm_estimado_MC)
abline(b=1, a = 0, col = 2)
