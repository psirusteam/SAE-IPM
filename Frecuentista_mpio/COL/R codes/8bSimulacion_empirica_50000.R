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
source("Frecuentista_depto/0Funciones/funciones_mrp.R",
       encoding = "UTF-8")
source("Frecuentista_depto/0Funciones/funciones_simulacion.R",
       encoding = "UTF-8")
###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(25000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## leer encuesta
encuesta <- readRDS("Frecuentista_depto/COL/Data/encuesta_ipm.rds")
n_sample <- 50000
###--- Censo: Completo ---###

pseudocenso <- readRDS("Frecuentista_depto/COL/Data/pseudocenso.rds")
Censo_agregado <- readRDS("Frecuentista_depto/COL/Data/censo_ipm2.rds")
tasa_desocupados <- readRDS("Frecuentista_depto/COL/Data/tasa_desocupacion.rds")
statelevel_predictors_df <- tasa_desocupados

poststrat_df <- inner_join(Censo_agregado,
                           statelevel_predictors_df, by ="depto") %>% 
  mutate(sexo= as.character(sexo)) %>% 
  mutate_at(vars(matches("ipm_")), as.character)

byAgrega <- c("depto", "area", "sexo", "edad",
              "ipm_Material",
              "ipm_Hacinamiento",
              "ipm_Agua",
              "ipm_Saneamiento",
              "ipm_Energia",
              "ipm_Internet" )

encuesta_agg <- encuesta %>% 
  mutate(condact3 = as.character(condact3)) %>% 
  filter(!is.na(condact3))  %>%
  group_by_at(byAgrega) %>%
  summarise(nd = n(), .groups = "drop") %>% 
  mutate_at(all_of(byAgrega), as.character)


## Estimacion censal.
ipm_censo <- pseudocenso %>% group_by(depto) %>% 
  summarise(ipm_depto = mean(ipm_depto))


for(ii in 1:100){  
  poststrat_temp  <- poststrat_df
  
  encuesta_MC <- pseudocenso %>% sample_n(size = n_sample)
    
  encuesta_MC %<>%
    group_by_at(all_of(byAgrega)) %>%
    summarise(n = n(),
              No_educacion = sum(y_educacion),
              No_empleo = sum(y_empleo),
              .groups = "drop")
  
  
  
  encuesta_MC <- inner_join(encuesta_MC, 
                            statelevel_predictors_df,
                            by = c("depto"))
  plan(multisession, workers = 2)
  
  fit_MC <- future_map(
    setNames(c("No_educacion", "No_empleo"),
             c("fit_educion_MC", "fit_emplo_MC")),
    ~ modelo(y_si = .x, setdata = encuesta_MC)
  )
  
  
  poststrat_temp$prob_boot_educacion <- predict(
    fit_MC$fit_educion_MC,
    newdata = poststrat_temp,
    type = "response",
    allow.new.levels = TRUE
  )
  
  poststrat_temp$prob_boot_empleo <- predict(
    fit_MC$fit_emplo_MC,
    newdata = poststrat_temp,
    type = "response",
    allow.new.levels = TRUE
  )
  
  
  poststrat_boot_MC <- poststrat_temp %>% data.frame() %>% 
    dplyr::select(depto, n,matches("prob_boot|ipm_") ) %>% 
    tibble()
  
  poststrat_boot_MC %<>% mutate_at(vars(matches("ipm_")), as.numeric) 
  
  
  ipm_MC <- replicate(20,fipm(poststrat_boot_MC))
  
  cat("Termina MC ii = ", ii, "...... OK\n")
  
  ipm_estimado_MC <- map_df(1:ncol(ipm_MC),
                            function(x)
                              data.frame(t(ipm_MC)[x, ])) %>%
    group_by(depto) %>% summarise(ipm_estimado = mean(ipm))
  
  ipm_estimado_MC <-
    inner_join(ipm_estimado_MC, ipm_censo, by = "depto")
  
  plot(ipm_estimado_MC$ipm_estimado, ipm_estimado_MC$ipm_depto)
  abline(b = 1,a=0)
  saveRDS(object = ipm_estimado_MC, 
          file = paste0("Frecuentista_depto/COL/Data/b_50000_iter_IPM_empirica/MC_",
                        ii, ".rds"))
  
  
  cat(paste0(rep("-", 60), collapse = ""),"\n")
  cat("###### Ternino el MC ", ii , "######\n")
  cat(paste0(rep("-", 60), collapse = ""),"\n")
  
}





## leer estimacion MC del IPM  
ipm_MC <- readRDS(file = "Frecuentista_depto/COL/Data/ipm_MC.rds")


list.files("Frecuentista_depto/COL/Data/b_50000_iter_IPM_empirica/",full.names = TRUE
) %>% 
  map_df(~readRDS(.x) ) %>% 
  group_by(depto) %>% 
  summarise(
    sesgo = 100*mean(ipm_depto - ipm_estimado),
    smce = sqrt(mean(c(ipm_estimado-ipm_depto)^2)),
    cve = 100*smce/mean(ipm_estimado)) %>% 
  as.data.frame() %>% 
  openxlsx::write.xlsx( 
    "Frecuentista_depto/COL/Output/Tablas_resultado_teorico/simu_muestreo_MAS_50000.xlsx")


