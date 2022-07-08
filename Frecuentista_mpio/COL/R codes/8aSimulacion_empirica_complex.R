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
select <- dplyr::select
source("Frecuentista_depto/Funciones/funciones_mrp.R",
       encoding = "UTF-8")

fipm <- function(data_MC_boot){
  iter_ipm_boot <-  pmap(as.list(data_MC_boot),
                         function(
    depto,n,
    ipm_Material,
    ipm_Saneamiento,
    ipm_Energia,
    ipm_Internet,
    ipm_Agua,
    ipm_Hacinamiento,
    prob_boot_educacion,
    prob_boot_empleo
                         ) {
                           y_empleo =  rbinom(n, 1, prob = prob_boot_empleo)
                           y_educacion =  rbinom(n, 1, prob = prob_boot_educacion)
                           ipm <- 0.1 * (
                             ipm_Material+
                               ipm_Saneamiento+
                               ipm_Energia+
                               ipm_Internet+
                               ipm_Agua+
                               ipm_Hacinamiento) +
                             0.2 * (y_educacion +   y_empleo) 
                           
                           ipm_dummy <- ifelse(ipm < 0.4, 0, 1) 
                           mean(ipm_dummy)
                         })
  
  data_MC_boot$ipm <- unlist(iter_ipm_boot) 
  
  data_MC_boot %>% group_by(depto) %>%
    summarise(ipm = sum((n*ipm))/sum(n))
}

modelo <- function(y_si, setdata) {
  setdata$y_si <- setdata[[y_si]]
  setdata$y_no <- setdata$n - setdata[[y_si]]
  cat("Modelo para ", y_si, "\n")
  glmer(
    cbind(y_si, y_no) ~ (1 | depto) +
      edad +
      area +
      ipm_Material+
      ipm_Hacinamiento+
      ipm_Agua+
      ipm_Saneamiento +
      ipm_Energia + 
      ipm_Internet +
      sexo  + tasa_desocupacion +
      F182013_stable_lights + 
      X2016_crops.coverfraction +
      X2016_urban.coverfraction  ,
    family = binomial(link = "logit"),
    data = setdata,
  )
}




###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

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


ipm_censo <- poststrat_temp %>% unnest(iter_ipm2) %>% 
  group_by(depto) %>% 
  summarise(ipm_depto = mean(ipm_depto))


# Seleccionando las muestra del censo ------------------------------------

encuesta_boot_agg <- poststrat_temp %>%
  inner_join(encuesta_agg, by = byAgrega) %>%
  select(all_of(byAgrega), nd, iter_ipm2)

encuesta_boot_agg %<>% unnest(iter_ipm2) %>%
  group_by_at(all_of(c(byAgrega,"nd"))) %>%
  nest()

# encuesta_boot_agg %>% mutate(Nd = map_dbl(data, nrow)) %>% 
#   filter(nd>Nd)
for(ii in 1:1000){  
  plan(multisession, workers = 2)
  encuesta_MC <- encuesta_boot_agg %>% mutate(
    muestra = map2(nd,
                   data, 
                   function(x,y)sample_n(tbl = y, size = x, replace = TRUE)),
    data = NULL) %>% unnest(muestra)
  
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
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )
  
  poststrat_temp$prob_boot_empleo <- predict(
    fit_MC$fit_emplo_MC,
    newdata = poststrat_df,
    type = "response",
    allow.new.levels = TRUE
  )
  
  
  poststrat_boot_MC <- poststrat_temp %>% data.frame() %>% 
    dplyr::select(depto, n,matches("prob_boot|ipm_") ) %>% 
    tibble()
  
  poststrat_boot_MC %<>% mutate_at(vars(matches("ipm_")), as.numeric) 
  
  
  ipm_MC <- replicate(20,fipm(poststrat_boot_MC))
  
  cat("Termina MC ii = ", ii, "...... OK\n")
  
  ipm_estimado_MC <- map_df(1:ncol(ipm_MC), function(x) data.frame(t(ipm_MC)[x,])) %>%
    group_by(depto) %>% summarise(ipm_estimado = mean(ipm))
  
  ipm_estimado_MC <- inner_join(ipm_estimado_MC, ipm_censo, by = "depto")
  
  saveRDS(object = ipm_estimado_MC, 
          file = paste0("Frecuentista_depto/COL/Data/iter_IPM_empirica/MC_",
                        ii, ".rds"))
  
  
  cat(paste0(rep("-", 60), collapse = ""),"\n")
  cat("###### Ternino el MC ", ii , "######\n")
  cat(paste0(rep("-", 60), collapse = ""),"\n")
  
}


## leer estimacion MC del IPM  
ipm_MC <- readRDS(file = "Frecuentista_depto/COL/Data/ipm_MC.rds")


list.files("Frecuentista_depto/COL/Data/iter_IPM_empirica/",
  full.names = TRUE ) %>% 
  map_df(~readRDS(.x) ) %>% 
  mutate(diff = ipm_estimado - ipm_depto) %>% 
  group_by(depto) %>% 
  summarise(smce = sqrt(mean(diff^2))) %>% 
  inner_join(ipm_MC) %>%
  mutate(cve = (smce/ipm_estimado_MC)*100,
         Lim_Inf = ipm_estimado_MC - 1.96*smce,
         Lim_Sup = ipm_estimado_MC + 1.96*smce,         
         ) %>% 
  openxlsx::write.xlsx(
    file = "Frecuentista_depto/COL/Output/Tablas_resultado_teorico/ipm_COL.xlsx")
  
list.files("Frecuentista_depto/COL/Data/iter_IPM_empirica/",
           full.names = TRUE) %>%
  map_df( ~ readRDS(.x)) %>%
  group_by(depto) %>% summarise(
    censo_ipm = mean(ipm_depto),
    media_estimacion_ipm = mean(ipm_estimado),
    smce = sqrt(mean((ipm_estimado - ipm_depto)^2)),
    sd_estimacion = sd(ipm_estimado)
  ) %>% data.frame() %>% 
  inner_join(ipm_MC) %>% 
  openxlsx::write.xlsx(
    file = "Frecuentista_deptoCOL/Output/smce_MC.xlsx")

