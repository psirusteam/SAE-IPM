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
library(tibble)
library(openxlsx)
select <- dplyr::select
source("Frecuentista_depto/0Funciones/funciones_mrp.R",
       encoding = "UTF-8")


###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Leer modelos
fit_empleo <- readRDS("Frecuentista_depto//COL/Data/fit_freq_empleo.rds")
## leer encuesta
encuesta <- readRDS("Frecuentista_depto/COL/Data/encuesta_ipm.rds")
tasa_desocupados <- readRDS("Frecuentista_depto/COL/Data/tasa_desocupacion.rds")
statelevel_predictors_df <- tasa_desocupados

###--- Censo: Completo ---###

Censo_agregado <- readRDS("Frecuentista_depto/COL/Data/censo_ipm2.rds")

## leer estimacion MC del IPM  
ipm_MC <- readRDS(file = "Frecuentista_depto/COL/Data/ipm_empleo.rds")


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

poststrat_df$pred_empleo <-  predict(fit_empleo, poststrat_df,
                               allow.new.levels = TRUE)




## Efectos aleatorios 
var_u_empleo <- as.numeric(VarCorr(fit_empleo))

udm <- ranef(fit_empleo)$depto %>%
                 rename(udm_empleo = "(Intercept)") %>% 
                  rownames_to_column(var = "depto")

udm <- full_join(udm,
                 data.frame(depto = unique(poststrat_df$depto)),
                 by = "depto"
)

udm <- poststrat_df %>% distinct(depto) %>% 
  full_join(udm, by = "depto") %>% 
  mutate(udm_empleo = ifelse(is.na(udm_empleo), 0, udm_empleo)) 

# Bootstrap: Gonzalez  ----------------------------------------------------

encuesta_agg <- encuesta %>% 
  mutate(condact3 = as.character(condact3)) %>% 
  filter(!is.na(condact3))  %>%
  group_by_at(byAgrega) %>%
  summarise(nd = n(), .groups = "drop") %>% 
  mutate_at(all_of(byAgrega), as.character)

##### Iniciando el boot 

ii = 1
for(ii in 1:100){

ud <- udm %>%  mutate(
    udi_empleo = rnorm(1:n(), 0, sd = sqrt(var_u_empleo)) )  %>% data.frame()


poststrat_temp <- inner_join(poststrat_df, ud, by = "depto") %>% 
  mutate(prob.boot_empleo = boot::inv.logit(pred_empleo - udm_empleo +  udi_empleo))

## Resultados en el censo boot 
cat("Inicia el boot_ipm_censo ii = ", ii, "\n")

poststrat_temp %<>%
  mutate(empleo_MC = map2(n, prob.boot_empleo, function(ni, prob_e) {
    data.frame(
      y_empleo =  rbinom(ni, 1, prob = prob_e))
  })) 



boot_ipm_censo <- poststrat_temp %>% unnest(empleo_MC) %>% 
  group_by(depto) %>% 
  summarise(ipm_boot = mean(y_empleo))

cat("boot_ipm_censo ii = ", ii, "...... OK\n")


# Seleccionando muestra del censo boot ------------------------------------

encuesta_boot_agg <- poststrat_temp %>%
  inner_join(encuesta_agg, by = byAgrega) %>%
  select(all_of(byAgrega), nd, empleo_MC)

encuesta_boot_agg %<>% unnest(empleo_MC) %>%
  group_by_at(all_of(c(byAgrega,"nd"))) %>%
  nest()

# encuesta_boot_agg %>% mutate(Nd = map_dbl(data, nrow)) %>%
#   filter(nd>Nd)

encuesta_boot_agg %<>% mutate(
  muestra = map2(nd,
     data, 
     function(x,y)sample_n(tbl = y, size = x, replace = TRUE)),
data = NULL) %>% unnest(muestra)

encuesta_boot_agg %>% group_by(depto) %>% 
  summarise(ipm_empleo = mean(1-y_empleo))

encuesta_boot_agg %<>%   
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            No_empleo = sum(1-y_empleo),
            Empleo = n - No_empleo,
            .groups = "drop")


encuesta_boot_agg <- inner_join(encuesta_boot_agg, 
                              statelevel_predictors_df,
                              by = c("depto"))

fit_boot_empleo <- glmer(
  cbind(Empleo, No_empleo) ~  (1 | depto) +
    edad +
    area +
    # anoest +
    # etnia +
    # depto:area +
    # depto:etnia +
    # depto:sexo +
    # depto:edad +
    # depto:anoest +
    # area:etnia +
    # area:sexo +
    # area:edad +
    # area:anoest +
  # etnia:sexo +
  # etnia:edad +
  # etnia:anoest +
  # sexo:edad +
  # sexo:anoest +
  # edad:anoest +
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
  data = encuesta_boot_agg
)

cat("fit_boot_empleo ii = ", ii, "...... OK\n")

poststrat_temp$prob_ipm <- predict(
  fit_boot_empleo,
  newdata = poststrat_df,
  type = "response",
  allow.new.levels = TRUE
)

poststrat_boot_MC <- poststrat_temp %>% data.frame() %>% 
  dplyr::select(depto, n,prob_ipm ) %>% 
  tibble()


rep_ipm_MC <- function(data_MC_boot){
  data_MC_boot %>% mutate(
    ipm_MC = map2_dbl(n,prob_ipm, function(ni, prob_e){
      y_ipm =  rbinom(ni, 1, prob = prob_e) 
      mean(y_ipm)
    })) %>% group_by(depto) %>%
    summarise(ipm_MC = sum((n*ipm_MC))/sum(n))    
}

cat("Inicia MC ii = ", ii, "\n")

empleo_MC <- replicate(20,rep_ipm_MC(poststrat_boot_MC))

cat("Termina MC ii = ", ii, "...... OK\n")

ipm_empleo_MC <- map_df(1:ncol(empleo_MC), function(x) data.frame(t(empleo_MC)[x,])) %>%
  group_by(depto) %>% summarise(ipm_empleo_estimado_MC = mean(ipm_MC))

ipm_empleo_MC <- inner_join(ipm_empleo_MC, boot_ipm_censo, by = "depto")

saveRDS(object = ipm_empleo_MC, 
        file = paste0("Frecuentista_depto/COL/Data/iter_empleo_BOOT_MC/boot_",
                      ii, ".rds"))


cat(paste0(rep("-", 60), collapse = ""),"\n")
cat("###### Ternino el boot ", ii , "######\n")
cat(paste0(rep("-", 60), collapse = ""),"\n")

}


ipm_MC <- readRDS(file = "Frecuentista_depto/COL/Data/ipm_empleo.rds")


list.files("Frecuentista_depto/COL/Data/iter_empleo_BOOT_MC/",full.names = TRUE
) %>% 
  map_df(~readRDS(.x) ) %>% 
  mutate(diff = ipm_empleo_estimado_MC - ipm_boot) %>% 
  group_by(depto) %>% summarise(smce = sqrt(mean(diff^2))) %>% 
  inner_join(ipm_MC) %>% 
  openxlsx::write.xlsx(
    file = "Frecuentista_depto/COL/Output/empleo_smce_MC.xlsx", overwrite = TRUE)

