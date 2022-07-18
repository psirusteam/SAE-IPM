#########################################################
# Proyecto IPM                   #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez   & Hanwen ZHANG   #
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

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Estimacion directa de las dimensiones educacion, empleo y ipm
Estimacion_dir <- readRDS(
  file = "Frecuentista_depto/COL/Data/Educacion_Empleo_IPM_dir.rds") %>% 
  rename_at(vars(Educacion:IPM_se), ~paste0("Dir_",.))

# Estimacion montecarlo para las dimensiones educacion, empleo y ipm

Estimacion_MC <- full_join(
readRDS(file = "Frecuentista_depto/COL/Data/ipm_educacion.rds") %>% 
  rename(sae_MC_Educacion = ipm_educacion_estimado_MC),
readRDS(file = "Frecuentista_depto/COL/Data/ipm_empleo.rds")%>% 
  rename(sae_MC_Empleo = ipm_empleo_estimado_MC)) %>% 
full_join(
    readRDS(file = "Frecuentista_depto/COL/Data/ipm_MC.rds")%>% 
  rename(sae_MC_IPM = ipm_estimado_MC))


# Estimación smce (boot) para dimensiones educacion, empleo y ipm
# smce empleo 
smce_empleo <- 
list.files("Frecuentista_depto/COL/Data/iter_empleo_BOOT_MC/",
           full.names = TRUE) %>% 
  map_df(~readRDS(.x) ) %>% 
  mutate(diff = ipm_empleo_estimado_MC - ipm_boot) %>% 
  group_by(depto) %>% summarise(smce_empleo = sqrt(mean(diff^2)))
# smce educacion 
smce_educacion <- 
  list.files("Frecuentista_depto/COL/Data/iter_educacion_BOOT_MC/",
             full.names = TRUE ) %>% 
  map_df(~readRDS(.x) ) %>% 
  mutate(diff = ipm_educacion_estimado_MC - ipm_boot) %>% 
  group_by(depto) %>% summarise(smce_educacion = sqrt(mean(diff^2)))
# smce ipm 
smce_ipm <- 
  list.files("Frecuentista_depto/COL/Data/iter_IPM_BOOT_MC/",
             full.names = TRUE) %>% 
  map_df(~readRDS(.x) ) %>% 
  mutate(diff = ipm_boot_estimado - ipm_boot) %>% 
  group_by(depto) %>% summarise(smce_IPM = sqrt(mean(diff^2)))

Estimacion_smce <-full_join(smce_empleo, smce_educacion) %>%
  full_join(smce_ipm)

##### Organizando el excel 
Estimacion <- full_join(Estimacion_dir,Estimacion_MC) %>% 
  full_join(Estimacion_smce) 

Estimacion %>%
  mutate(
    Educacion_cv = (smce_educacion / sae_MC_Educacion) * 100,
    Empleo_cv = (smce_empleo / sae_MC_Empleo) * 100,
    IPM_cv = (smce_IPM / sae_MC_IPM) * 100,
    Educacion_LimI = sae_MC_Educacion - 1.96 * smce_educacion,
    Educacion_LimS = sae_MC_Educacion + 1.96 * smce_educacion,
    Empleo_LimI = sae_MC_Empleo - 1.96 * smce_empleo,
    Empleo_LimS = sae_MC_Empleo + 1.96 * smce_empleo,
    IPM_LimI = sae_MC_IPM - 1.96 * smce_IPM,
    IPM_LimS = sae_MC_IPM + 1.96 * smce_IPM
  ) %>% 
  openxlsx::write.xlsx(
    file = "Frecuentista_depto/COL/Output/Comparando_dir_censo_sae/Estimacion_depto.xlsx")
