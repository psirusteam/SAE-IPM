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
select <- dplyr::select
source("Frecuentista_mpio/0Funciones/funciones_mrp.R",
       encoding = "UTF-8")
source("Frecuentista_mpio/0Funciones/aux_ipm.R",
       encoding = "UTF-8")


###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Leer modelos
fit_educacion <- readRDS("Frecuentista_mpio//COL/Data/fit_freq_educacion.rds")
fit_empleo <- readRDS("Frecuentista_mpio//COL/Data/fit_freq_empleo.rds")

## leer encuesta
encuesta <- readRDS("Frecuentista_mpio/COL/Data/encuesta_ipm.rds")

###--- Censo: Completo ---###

Censo_agregado <- readRDS("Frecuentista_mpio/COL/Data/censo_ipm2.rds")
tasa_desocupados <- readRDS("Frecuentista_mpio/COL/Data/tasa_desocupacion.rds")
statelevel_predictors_df <- tasa_desocupados


byAgrega <- c("mpio", "area", "sexo", "edad",
              "ipm_Material",
              "ipm_Hacinamiento",
              "ipm_Agua",
              "ipm_Saneamiento",
              "ipm_Energia",
              "ipm_Internet" )


poststrat_df <- left_join(Censo_agregado %>% mutate_at(all_of(byAgrega), as.character), 
                          statelevel_predictors_df,
                          by = c("mpio"))

poststrat_df$pred_educacion <-  predict(fit_educacion, poststrat_df,
                               allow.new.levels = TRUE)

poststrat_df$pred_empleo <-  predict(fit_empleo, poststrat_df,
                                        allow.new.levels = TRUE)


## Efectos aleatorios 
var_u_educacion <- as.numeric(VarCorr(fit_educacion))
var_u_empleo <- as.numeric(VarCorr(fit_empleo))

udm_1 <- ranef(fit_educacion)$mpio %>%
                 rename(udm_educacion = "(Intercept)") %>% 
                  tibble::rownames_to_column(var = "mpio")
udm_2 <- ranef(fit_empleo)$mpio %>%
  rename(udm_empleo = "(Intercept)") %>% 
  tibble::rownames_to_column(var = "mpio")

udm <- full_join(udm_1, udm_2, by = "mpio")

udm <- full_join(udm,
                 data.frame(mpio = unique(poststrat_df$mpio)),
                 by = "mpio"
)

udm <- poststrat_df %>% distinct(mpio) %>% 
  full_join(udm, by = "mpio") %>% 
  mutate(udm_educacion = ifelse(is.na(udm_educacion), 0, udm_educacion),
         udm_empleo = ifelse(is.na(udm_empleo), 0, udm_empleo)) 

# Bootstrap: Gonzalez  ----------------------------------------------------

encuesta_agg <- encuesta %>% 
  mutate(condact3 = as.character(condact3)) %>% 
  filter(!is.na(condact3))  %>%
  group_by_at(byAgrega) %>%
  summarise(nd = n(), .groups = "drop") %>% 
  mutate_at(all_of(byAgrega), as.character)

##### Iniciando el boot 
ii= 1
for(ii in 1:100){

ud <- udm %>%  mutate(
    udi_educacion = rnorm(1:n(), 0, sd = sqrt(var_u_educacion)),
    udi_empleo = rnorm(1:n(), 0, sd = sqrt(var_u_empleo))
  )  %>% data.frame()


poststrat_temp <- inner_join(poststrat_df, ud, by = "mpio") %>% 
  mutate(prob.boot_educaicion = boot::inv.logit(pred_educacion - udm_educacion +  udi_educacion), 
         prob.boot_empleo = boot::inv.logit(pred_empleo - udm_empleo +  udi_empleo))

poststrat_MC <- poststrat_temp %>% data.frame() %>% 
  dplyr::select(mpio, n,matches("prob|ipm_") ) %>% 
  tibble()

poststrat_MC %<>% mutate_at(vars(matches("ipm_")), as.numeric) 
## Resultados en el censo boot 
cat("Inicia el boot_ipm_censo ii = ", ii, "\n")
iter_ipm2 <-  pmap(as.list(poststrat_MC),
                   function(
                     mpio,n,
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
                       ipm_mpio = ipm_dummy,
                       y_educacion = y_educacion,
                       y_empleo = y_empleo 
                       )
                   })

poststrat_temp %<>% mutate(iter_ipm2 = iter_ipm2)
rm(poststrat_MC, iter_ipm2)


boot_ipm_censo <- poststrat_temp %>% unnest(iter_ipm2) %>% 
  group_by(mpio) %>% 
  summarise(ipm_boot = mean(ipm_mpio))
cat("boot_ipm_censo ii = ", ii, "...... OK\n")


# Seleccionando muestra del censo boot ------------------------------------

encuesta_boot_agg <- poststrat_temp %>%
  inner_join(encuesta_agg, by = byAgrega) %>%
  select(all_of(byAgrega), nd, iter_ipm2)

encuesta_boot_agg %<>% unnest(iter_ipm2) %>%
  group_by_at(all_of(c(byAgrega,"nd"))) %>%
  nest()

# encuesta_boot_agg %>% mutate(Nd = map_dbl(data, nrow)) %>% 
#   filter(nd>Nd)

encuesta_boot_agg %<>% mutate(
  muestra = map2(nd,
     data, 
     function(x,y)sample_n(tbl = y, size = x, replace = TRUE)),
data = NULL) %>% unnest(muestra)

encuesta_boot_agg %>% group_by(mpio) %>% 
  summarise(ipm_tem = mean(ipm_mpio))

encuesta_boot_agg %<>%
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            No_educacion = sum(y_educacion),
            Educacion = n - No_educacion,
            No_empleo = sum(y_empleo),
            Empleo = n - No_empleo,
            .groups = "drop")



# encuesta_boot_agg <- encuesta %>%
#   group_by_at(all_of(byAgrega)) %>%
#   summarise(n = n(),
#             No_educacion = sum(1-ipm_educacion),
#             Educacion = n - No_educacion,
#             No_empleo = sum(1-ipm_Empleo_Aseguramiento),
#             Empleo = n - No_empleo,
#             .groups = "drop")

encuesta_boot_agg <- inner_join(encuesta_boot_agg, 
                              statelevel_predictors_df,
                              by = c("mpio"))
plan("future::multisession")
fit_MC <- future_imap(
  setNames(c("No_educacion", "No_empleo"),
           c("fit_educion_MC", "fit_emplo_MC")),
  ~ modelo(y_si = .x, setdata = encuesta_boot_agg),
  .progress = TRUE
)
## Validar coefcientes 
 # fit_MC$fit_educion_MC
 # fit_educacion
 # fit_MC$fit_emplo_MC
 # fit_empleo



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
  dplyr::select(mpio, n,matches("prob_boot|ipm_") ) %>% 
  tibble()

poststrat_boot_MC %<>% mutate_at(vars(matches("ipm_")), as.numeric) 
cat("Inicia MC ii = ", ii, "\n")

plan("future::multisession", workers = 4)
ipm_MC <- furrr::future_imap(1:20,~aux_ipm(poststrat_boot_MC),
                                   .progress = TRUE)

cat("Termina MC ii = ", ii, "...... OK\n")

ipm_estimado_MC <- ipm_MC %>% bind_rows() %>%
  group_by(mpio) %>% summarise(ipm_boot_estimado = mean(ipm))

ipm_estimado_MC <- inner_join(ipm_estimado_MC, 
                              boot_ipm_censo, by = "mpio")

saveRDS(object = ipm_estimado_MC, 
        file = paste0("Frecuentista_mpio/COL/Data/iter_IPM_BOOT_MC/boot_",
                      ii, ".rds"))


cat(paste0(rep("-", 60), collapse = ""),"\n")
cat("###### Ternino el boot ", ii , "######\n")
cat(paste0(rep("-", 60), collapse = ""),"\n")

}


# ## leer estimacion MC del IPM  
# ipm_MC <- readRDS(file = "Frecuentista_mpio/COL/Data/ipm_MC.rds")
# 
# 
# list.files("Frecuentista_mpio/COL/Data/iter_IPM_BOOT_MC/",full.names = TRUE
# ) %>% 
#   map_df(~readRDS(.x) ) %>% 
#   mutate(diff = ipm_boot_estimado - ipm_boot) %>% 
#   group_by(mpio) %>% summarise(smce = sqrt(mean(diff^2))) %>% 
#   inner_join(ipm_MC) %>% 
#   openxlsx::write.xlsx(
#     file = "Frecuentista_mpio/COL/Output/smce_MC.xlsx")
