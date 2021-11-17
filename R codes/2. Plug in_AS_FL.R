
###--- Cleaning R environment ---###

rm(list = ls())
gc()

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
library(StatisticalModels)
library(fastDummies)
library(haven)
library(magrittr)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(180000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################

###--- GEIH: Completa ---###

GEIH2018 <- readRDS("Input/1. Data/GEIH2018.rds")

###--- GEIH: con variables estandarizadas ---###

Xencuesta <- readRDS("Input/1. Data/Xencuesta.rds")

###--- Censo: Completo ---###

CensoPersonas <- readRDS("Input/1. Data/Censo_Completo.rds")

###--- Censo: con variables estandarizadas ---###

Xcenso <- readRDS("Input/1. Data/Xcenso.rds")

################################################################################
              ### GEIH: Creando las dimensiones de interés ###
################################################################################

###--------------- Aseguramiento Salud y formalidad laboral -----------------###

### p6090. ¿ ... está afiliado, es cotizante o es  beneficiario de alguna    ### 
### entidad de seguridad social en salud? (Instituto de Seguros Sociales     ###
### - ISS, Empresa Promotora de Salud - EPS o Administradora de Régimen      ###
### Subsidiado - ARS) 1 Sí 2 No 9 No sabe, no informa                        ###

### p6920: ¿Está... cotizando actualmente a un fondo de pensiones? 1 Sí 2 No ###
###        3 Ya es pensionado                                                ###

indicadores <- GEIH2018 %>% transmute(
  idhogar = paste0(directorio, secuencia_p),
  Aseguramiento_Salud = case_when(edad > 5 & p6090 == 1 ~ 1,
                                  edad > 5 & p6090 == 2 ~ 0,
                                  TRUE ~ NA_real_),
  Formalidad_laboral = case_when(edad > 17 & condact3 == 1 & p6920 %in% c(1,3) ~ 1,
                                 edad > 17 & condact3 == 1 & p6920 %in% c(2) ~ 0,
                                 TRUE ~ NA_real_))

table(GEIH2018$condact3)
###--------------- Anexando variables a la base estandarizada ---------------###

indAS <- subset(indicadores, Aseguramiento_Salud %in% c(0,1))
table(indAS$Aseguramiento_Salud, useNA = "a")

indFL <- subset(indicadores, Formalidad_laboral %in% c(0,1))
table(indFL$Formalidad_laboral, useNA = "a")

# Xencuesta1 %<>% left_join(indicadores %>% group_by(idhogar) %>%
#                summarise(Priv_aseguramiento_salud = ifelse(any(Aseguramiento_Salud %in% 0), 1, 0),
#                          Priv_Formalidad_laboral = ifelse(any(Formalidad_laboral %in% 0), 1, 0)), by = "idhogar")

Xencuesta1 <- Xencuesta %>% right_join(indAS %>% group_by(idhogar) %>%
                           summarise(Priv_AS = ifelse(any(Aseguramiento_Salud %in% 0),
                                                      1, 0), by = "idhogar"))
table(Xencuesta1$Priv_AS, useNA = "a")

Xencuesta2 <- Xencuesta %>% right_join(indFL %>% group_by(idhogar) %>%
                           summarise(Priv_FL = ifelse(any(Formalidad_laboral %in% 0),
                                                      1, 0), by = "idhogar"))

table(Xencuesta2$Priv_FL, useNA = "a")

################################################################################
###------------ Ajuste del modelo Plugin: aseguramiento en salud ------------###
################################################################################

paste(names(Xencuesta), collapse = " + ")
pluginreg_1 <- glmer(Priv_aseguramiento_salud ~ Area + tipo_viv_casa + 
                     tipo_viv_depto + tipo_viv_cuarto + tipo_viv_indigena + 
                     mpared1 + mpared2 + mpared3 + mpared4 + mpared5 + mpared6 +
                     mpisos1 + mpisos2 + mpisos3 + mpisos4 + mpisos5 +
                     agua_alim1 + agua_alim2 + agua_alim3 + agua_alim4 +
                     agua_alim5 + agua_alim6 + jefe_Mujer + prop_mujeres +
                     electrica_ee + acueducto + nhijos_hogar2 + nhijos_hogar3 +
                     nhijos_hogar4 + alcantarillado + prop_alfabeta +
                     gasnatural_redp + rec_basura + internet + servhig1 + 
                     servhig2 + servhig3 + servhig4 + servhig5 + tamhog2 +
                     tamhog3 + tamhog4 + hacinamiento + prep_alim2 + prep_alim3 +
                     prep_alim4 + prep_alim5 + prep_alim6 + Edad_jefe1 +
                     Edad_jefe2 + Edad_jefe3 + Edad_jefe4 + inasistente +  
                     Jefe_sup + ratio_prim + ratio_media + ratio_sup + 
                     prop_ocupados + prop_inactivos + trabajo_infantil +
                     r_solt + r_casad + migrante_cortop + migrante_medianop +
                     (1|Municipio), family = "binomial", data = Xencuesta1)

################################################################################
###-------------- Ajuste del modelo Plugin: Formalidad laboral --------------###
################################################################################

pluginreg_2 <- glmer(Priv_Formalidad_laboral ~ Area + tipo_viv_casa + 
                     tipo_viv_depto + tipo_viv_cuarto + tipo_viv_indigena + 
                     mpared1 + mpared2 + mpared3 + mpared4 + mpared5 + mpared6 +
                     mpisos1 + mpisos2 + mpisos3 + mpisos4 + mpisos5 + agua_alim1 +
                     agua_alim2 + agua_alim3 + agua_alim4 + agua_alim5 + 
                     agua_alim6 + jefe_Mujer + prop_mujeres + electrica_ee + 
                     acueducto + nhijos_hogar2 + nhijos_hogar3 + nhijos_hogar4 + 
                     alcantarillado + prop_alfabeta + gasnatural_redp + 
                     rec_basura + internet + servhig1 + servhig2 + servhig3 +
                     servhig4 + servhig5 + tamhog2 + tamhog3 + tamhog4 +
                     hacinamiento + prep_alim2 + prep_alim3 + prep_alim4 +
                     prep_alim5 + prep_alim6 + Edad_jefe1 + Edad_jefe2 + 
                     Edad_jefe3 + Edad_jefe4 + inasistente + Jefe_sup + 
                     ratio_prim + ratio_media + ratio_sup + prop_ocupados +
                     prop_inactivos + trabajo_infantil + r_solt + r_casad +
                     migrante_medianop + migrante_cortop + (1|Municipio),
                     family = "binomial", data = Xencuesta2)

#------------------------------------------------------------------------------#
#----------------- Exportando salidas: Modelo Plugin ajustado -----------------#
#------------------------------------------------------------------------------#

###--- Modelo para aseguramiento en salud ---###

saveRDS(pluginreg_1, file = "PluginSaludMunicipio.rds")

###--- Modelo para formalidad laboral ---###

saveRDS(pluginreg_2, file = "PluginLaboralMunicipio.rds")

#------------------------------------------------------------------------------#
#-------------------- Cargando los modelos Plugin ajustados -------------------#
#------------------------------------------------------------------------------#

###--- Modelo para aseguramiento en salud ---###

pluginreg_1 <- readRDS("PluginSaludMunicipio.rds")

###--- Modelo para formalidad laboral ---###

pluginreg_2 <- readRDS("PluginLaboralMunicipio.rds")

###--- Número de municipios en la encuesta ---###

sum(unique(Xencuesta$Municipio) %in% unique(Xcenso$Municipio))

#------------------------------------------------------------------------------#
#------------ Extracción de los efectos fijos comunes a los dominios ----------#
#------------------------------------------------------------------------------#

###--- Modelo para aseguramiento en salud ---###

betas1 = as.matrix(fixef(pluginreg_1))

###--- Modelo para formalidad laboral ---###

betas2 = as.matrix(fixef(pluginreg_2))

###------------ Construcción de la matriz censal sintética XBeta ------------###

matrizCenso <- cbind.data.frame(Xcenso$Municipio, cbind(1, as.matrix(Xcenso %>% 
                                select(rownames(betas1)[-1]))) %*% betas1,
                                cbind(1, as.matrix(Xcenso %>% select(rownames(betas2)[-1]))) %*% betas2)

colnames(matrizCenso) <- c("Municipio","XB1","XB2")
head(matrizCenso)

###------- Creando el vector nulo para ser reemplazado en el ciclo de -------### 
###-------                         estimación Plugin                  -------###

Xcenso$pluginSalud = numeric(14243219)
Xcenso$pluginFormal = numeric(14243219)

###--- Códigos de los municipios en el censo para el ciclo de estimación Plugin --#

Div = unique(Xcenso$Municipio)

###---- Efectos fijos y efectos aleatorios para cada uno de los dominios ----###

ud =  data.frame(Municipio = as.numeric(rownames(ranef(pluginreg_1)$Municipio)), 
                 Salud = ranef(pluginreg_1)$Municipio[[1]],
                 Formal = ranef(pluginreg_2)$Municipio[[1]])

rownames(ud) <- NULL
ud$Municipio <- as.factor(ud$Municipio)

###--- Uniendo los efectos aleatorios estimados con el resto de municipios --###

ud = data.frame(Municipio = unique(Xcenso$Municipio)) %>% 
     left_join(ud, by = "Municipio")

ud$Salud[is.na(ud$Salud)] <- 0
ud$Formal[is.na(ud$Formal)] <- 0

###---- Estimación de la probabilidad de que una persona tenga necesidad  ---###
###----          en aseguramiento en salud y formalidad laboral           ---###

for(i in 1:length(Div)){
  print(i)
  index = which(Xcenso$Municipio == Div[i])
  
  Xcenso$pluginSalud[index] = exp(matrizCenso$XB1[index] + ud$Salud[i])/
                               (1 + exp(matrizCenso$XB1[index] + ud$Salud[i]))
  Xcenso$pluginFormal[index] = exp(matrizCenso$XB2[index] + ud$Formal[i])/
                                (1 + exp(matrizCenso$XB2[index] + ud$Formal[i]))

}

#------------------------------------------------------------------------------#
#--------------- Exportando salidas: Censo con estimación Plugin --------------#
#------------------------------------------------------------------------------#

saveRDS(Xcenso, file = "Xcenso_plugin.rds")

#------------------------------------------------------------------------------#
#------------ Cargue de base de datos: Censo con estimación Plugin ------------#
#------------------------------------------------------------------------------#

Xcenso <- readRDS("Xcenso_plugin.rds")

###---- Anexando la información de los resultados de los modelos al Censo ---###

CensoPersonas1 <- CensoPersonas %>% transmute(idhogar, Divipola, 
                                              Departamento = U_DPTO, 
                                              Area = UA_CLASE) 
                             
CensoPersonas1 <- subset(CensoPersonas1, edad >5) %>% left_join(Xcenso %>% 
                                                      transmute(idhogar, pluginSalud),
                                                      by = "idhogar")

CensoPersonas2 <- subset(CensoPersonas1, edad > 17 & ocupados == 1) %>%  
                left_join(Xcenso %>% transmute(idhogar, pluginFormal),
                          by = "idhogar")

table(CensoPersonas$ocupados)

################################################################################
###----- Estimación del porcentaje de personas con privación de acceso a ----###
###-----                    salud o formalidad laboral                   ----###
################################################################################

###--- Nacional ---###

CensoPersonas %>% summarise(PrivSalud = mean(pluginSalud, na.rm = T),
                            PrivFormal = mean(pluginFormal, na.rm = T))

###--- Departamental ---###

res_depto <- CensoPersonas %>% group_by(Departamento) %>% 
             summarise(PrivSalud = mean(pluginSalud, na.rm = T), 
                       PrivFormal = mean(pluginFormal, na.rm = T))

###--- Municipal ---###

res_Mun <- CensoPersonas %>% group_by(Divipola) %>% 
           summarise(PrivSalud = mean(pluginSalud, na.rm = T),
                     PrivFormal = mean(pluginFormal, na.rm = T))

################################################################################
###--------------- Guardando los resultados de los modelos SAE --------------###
################################################################################

###--- Nivel Departamental ---###

write.csv2(res_depto, "Depto.csv")

###--- Nivel Municipal ---###

write.csv2(res_Mun, "Mun.csv")
