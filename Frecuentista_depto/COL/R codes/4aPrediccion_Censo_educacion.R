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
library(purrr)
source("Frecuentista_depto/0Funciones/funciones_mrp.R", encoding = "UTF-8")

# Loading data ------------------------------------------------------------
memory.limit(10000000000000)
fit_educacion <- readRDS( file = "Frecuentista_depto/COL/Data/fit_freq_educacion.rds")

encuesta_ipm <- readRDS("Frecuentista_depto/COL/Data/encuesta_ipm.rds")
censo_ipm <- readRDS("Frecuentista_depto/COL/Data/censo_ipm2.rds") 
tasa_desocupados <- readRDS("Frecuentista_depto/COL/Data/tasa_desocupacion.rds")

# Agregando encuesta ------------------------------------------------------
statelevel_predictors_df <- tasa_desocupados
byAgrega <- c("depto", "area", "sexo", "edad",
              "ipm_Material",
              "ipm_Hacinamiento",
              "ipm_Agua",
              "ipm_Saneamiento",
              "ipm_Energia",
              "ipm_Internet" )

poststrat_df <- inner_join(censo_ipm %>% mutate_at(all_of(byAgrega), as.character),
                           statelevel_predictors_df)


poststrat_df$prob_educacion <- predict(
  fit_educacion,
  newdata = poststrat_df,
  type = "response",
  allow.new.levels = TRUE
)

poststrat_MC <- poststrat_df %>% data.frame() %>% 
  dplyr::select(depto, n,matches("prob") ) %>% 
  tibble()


MC_educacion <- function(){
poststrat_MC %>% mutate(
  educacion_MC = map2_dbl(n,prob_educacion, function(ni, prob_e){
    y_educacion =  rbinom(ni, 1, prob = prob_e) 
    mean(y_educacion)
  })) %>% group_by(depto) %>%
    summarise(educacion_MC = sum((n*educacion_MC))/sum(n))    
}


educacion_MC <- replicate(100,MC_educacion())

ipm_educacion <- map_df(1:ncol(educacion_MC), function(x) data.frame(t(educacion_MC)[x,])) %>%
group_by(depto) %>% summarise(ipm_educacion_estimado_MC = mean(educacion_MC))

saveRDS(ipm_educacion, 
    file = "Frecuentista_depto/COL/Data/ipm_educacion.rds")


