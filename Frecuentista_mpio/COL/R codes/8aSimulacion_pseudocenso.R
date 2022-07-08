#########################################################
# Proyecto IPM                   #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################
###--- Cleaning R environment ---###

rm(list = ls())
gc()
# Loading required libraries ----------------------------------------------
#################
### Libraries ###
#################

library(dplyr)
library(survey)
library(srvyr)
library(sae)
library(lme4)
library(data.table)
library(haven)
library(magrittr)
library(formula.tools)
library(remotes)
#library(StatisticalModels)
library(fastDummies)
library(haven)
library(magrittr)
library(openxlsx)
library(purrr)
library(furrr)
library(tidyr)
library(tibble)
select <- dplyr::select
source("Frecuentista_depto/Funciones/funciones_mrp.R",
       encoding = "UTF-8")
source("Frecuentista_depto/Funciones/funciones_simulacion.R",
       encoding = "UTF-8")


###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(2500000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Leer modelos
fit_educacion <- readRDS("Frecuentista_depto/COL/Data/fit_freq_educacion.rds")
fit_empleo <- readRDS("Frecuentista_depto/COL/Data/fit_freq_empleo.rds")

## leer encuesta
encuesta <- readRDS("Frecuentista_depto/COL/Data/encuesta_ipm.rds")

###--- Censo: Completo ---###

Censo_agregado <- readRDS("Frecuentista_depto/COL/Data/censo_ipm2.rds")
tasa_desocupados <- readRDS("Frecuentista_depto/COL/Data/tasa_desocupacion.rds")
statelevel_predictors_df <- tasa_desocupados


byAgrega <- c("depto", "area", "sexo", "edad",
              "ipm_Material",
              "ipm_Hacinamiento",
              "ipm_Agua",
              "ipm_Saneamiento",
              "ipm_Energia",
              "ipm_Internet" )


poststrat_df <- left_join(Censo_agregado %>% mutate_at(all_of(byAgrega), as.character), 
                          statelevel_predictors_df,
                          by = c("depto"))

poststrat_df$pred_educacion <-  predict(fit_educacion, poststrat_df,
                                        allow.new.levels = TRUE)

poststrat_df$pred_empleo <-  predict(fit_empleo, poststrat_df,
                                     allow.new.levels = TRUE)

summary(poststrat_df$pred_educacion)
## Efectos aleatorios 
var_u_educacion <- as.numeric(VarCorr(fit_educacion))
var_u_empleo <- as.numeric(VarCorr(fit_empleo))

udm_1 <- ranef(fit_educacion)$depto %>%
  rename(udm_educacion = "(Intercept)") %>% 
  rownames_to_column(var = "depto")
udm_2 <- ranef(fit_empleo)$depto %>%
  rename(udm_empleo = "(Intercept)") %>% 
  rownames_to_column(var = "depto")

udm <- full_join(udm_1, udm_2, by = "depto")

udm <- full_join(udm,
                 data.frame(depto = unique(poststrat_df$depto)),
                 by = "depto"
)

udm <- poststrat_df %>% distinct(depto) %>% 
  full_join(udm, by = "depto") %>% 
  mutate(udm_educacion = ifelse(is.na(udm_educacion), 0, udm_educacion),
         udm_empleo = ifelse(is.na(udm_empleo), 0, udm_empleo)) 

# Bootstrap: Gonzalez  ----------------------------------------------------

encuesta_agg <- encuesta %>% 
  mutate(condact3 = as.character(condact3)) %>% 
  filter(!is.na(condact3))  %>%
  group_by_at(byAgrega) %>%
  summarise(nd = n(), .groups = "drop") %>% 
  mutate_at(all_of(byAgrega), as.character)

##### creando pseudocenso 


ud <- udm %>%  mutate(
  udi_educacion = rnorm(1:n(), 0, sd = sqrt(var_u_educacion)),
  udi_empleo = rnorm(1:n(), 0, sd = sqrt(var_u_empleo))
)  %>% data.frame()

# pred_educacion = Xtb + ud 
poststrat_temp <- inner_join(poststrat_df, ud, by = "depto") %>% 
  mutate(prob.boot_educaicion = boot::inv.logit(pred_educacion - udm_educacion +  udi_educacion), 
         prob.boot_empleo = boot::inv.logit(pred_empleo - udm_empleo +  udi_empleo))

poststrat_MC <- poststrat_temp %>% data.frame() %>% 
  dplyr::select(depto, n,matches("prob|ipm_") ) %>% 
  tibble()

poststrat_MC %<>% mutate_at(vars(matches("ipm_")), as.numeric) 
## Resultados en el censo boot 

plan(multisession, workers = 4)

iter_ipm2 <-  pmap(as.list(poststrat_MC),
                   function(
    depto,n,
    ipm_Material,
    ipm_Saneamiento,
    ipm_Energia,
    ipm_Internet,
    ipm_Agua,
    ipm_Hacinamiento,
    prob.boot_educaicion,
    prob.boot_empleo
                   ) {
                     y_empleo =  rbinom(n, 1, prob = prob.boot_empleo)
                     y_educacion =  rbinom(n, 1, prob = prob.boot_educaicion)
                     ipm <- 0.1 * (
                       ipm_Material+
                         ipm_Saneamiento+
                         ipm_Energia+
                         ipm_Internet+
                         ipm_Agua+
                         ipm_Hacinamiento) +
                       0.2 * (y_educacion +   y_empleo) 
                     
                     ipm_dummy <- ifelse(ipm < 0.4, 0, 1) 
                     data.frame(
                       ipm_depto = ipm_dummy,
                       y_educacion = y_educacion,
                       y_empleo = y_empleo 
                     )
                   })


poststrat_temp %<>% mutate(iter_ipm2 = iter_ipm2)
rm(poststrat_MC, iter_ipm2)


pseudocenso <- poststrat_temp %>% unnest(cols = "iter_ipm2")

saveRDS(pseudocenso, file = "Frecuentista_depto/COL/Data/pseudocenso.rds")


