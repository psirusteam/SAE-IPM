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
source("Modelo_bayes_HxA/0funciones/Estimar_ipm.R")

################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

poststrat_df <- left_join(censo_ipm, statelevel_predictors_df,
                          by = c("dam", "dam2"))

################################################################################
## Lectura de variables dummy  
################################################################################

epred_mat_agua_dummy <-  readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_agua_dummy.rds")
epred_mat_educacion_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_educacion_dummy.rds")
epred_mat_empleo_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_empleo_dummy.rds")
epred_mat_energia_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_energia_dummy.rds")
epred_mat_hacinamiento_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_hacinamiento_dummy.rds")
epred_mat_internet_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_internet_dummy.rds")
epred_mat_material_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_material_dummy.rds")
epred_mat_saneamiento_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_saneamiento_dummy.rds")

chain_q  <- 0.1 * (
  epred_mat_material_dummy +
    epred_mat_hacinamiento_dummy +
    epred_mat_agua_dummy +
    epred_mat_saneamiento_dummy +
    epred_mat_energia_dummy + epred_mat_internet_dummy
) +
  0.2 * (epred_mat_educacion_dummy +
           epred_mat_empleo_dummy)

chain_Indicadora <- chain_q

chain_Indicadora[chain_Indicadora <= 0.4] <- 0
chain_Indicadora[chain_Indicadora != 0] <- 1


D <- rowSums(chain_Indicadora*poststrat_df$n)
Q <- rowSums(chain_q*poststrat_df$n)
chain_IPM <- (D*Q)/((sum(poststrat_df$n)^2)*8)

mean(chain_IPM)
sd(chain_IPM)

estime_IPM(poststrat = poststrat_df,
           chain_q,
           byMap = NULL, 
           n_dim = 8) %>% data.frame()

estime_IPM(poststrat = poststrat_df,
           chain_q,
           byMap = "dam", 
           n_dim = 8) %>% data.frame()

