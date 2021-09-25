################################################################################
###---------------------- Modelo SAE con Senate Weights ---------------------###
################################################################################

###--- Limpieza de memoria ---###

rm(list = ls())
gc()

#################
### Librerías ###
#################

library(survey)
library(srvyr)
library(nlme)
library(lme4)
library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(sampling)
library(magrittr)
select <- dplyr::select

###--- Definiendo la memoria RAM a emplear ---###

memory.limit(18000000)

###--- Definiendo el directorio de trabajo ---###

#setwd("C:/Users/user/Desktop/CEPAL")

###---------------------------- Encuesta original ---------------------------###

#encuesta <- readRDS("GEIH2018.rds") 
encuesta <- readRDS("Input/1. Data/GEIH2018.rds") %>% mutate(idhogar = paste0(directorio, 
            secuencia_p)) %>% filter(id_pers == 1) %>% 
            mutate(pobrezaEx = ifelse(pobreza == 1, 1, 0),
                   pobreza = ifelse(pobreza != 3, 1, 0),
            estrato = paste(mes, DEPARTAMENTO, divipola, clase.x, sep = "_" ),
            upm =  paste(estrato,segmento,sep = "_" ))

###----------------- Encuesta con variables estandarizadas ------------------###

#Xencuesta <-  readRDS("0. Data/Output/Xencuesta.rds") 
Xencuesta <-  readRDS("Xencuesta.rds") 

###----------------------------- Senate Weights -----------------------------###

Xencuesta$SWeights <- nrow(Xencuesta)*(encuesta$factorex/sum(encuesta$factorex))

###--------------- Quitando algunas variables dicotómicas: GEIH -------------###

Xencuesta %<>% select(-depto76, -tipo_viv_otro, -mpared7, -mpisos6, -servhig6,
                      -tamhog4, -prep_alim6, -agua_alim9, -agua_alim10, 
                      -Edad_jefe1, -Edad_jefe5, -nhijos_hogar4) %>%
               mutate(efectos = paste0(Municipio, "-", as.character(Area)))

###------------------ Seleccionando algunas variables: Censo ----------------###

#Xcenso <- readRDS("0. Data/Output/Xcenso.rds") %>% mutate("(Intercept)" = 1,
Xcenso <- readRDS("Xcenso.rds") %>% mutate("(Intercept)" = 1,
          Municipio = ifelse(as.numeric(as.character(Municipio)) < 10000, 
          paste0(0, as.character(Municipio)), as.character(Municipio)),
          efectos = paste0(Municipio, "-", as.character(Area))) %>%
          select("(Intercept)",names(Xencuesta)[-c(92,94)])

###---------------------- Exportando las bases de datos ---------------------###

#saveRDS(Xencuesta,"5. Bootstrap/Output/XencuestaBootstrap.rds")
#saveRDS(Xcenso, "5. Bootstrap/Output/XcensoBootstrap.rds")
saveRDS(Xencuesta, "XencuestaBootstrap.rds")
saveRDS(Xcenso, "XcensoBootstrap.rds")

################################################################################
###------------------- Tamaños muestrales y poblacionales -------------------###
###-------------------     Dominios: Municipio - área     -------------------###
################################################################################

###-------- Tamaños muestrales --------###

n_d = Xencuesta %>% group_by(efectos) %>% summarise(n = n()) %>%
      right_join(data.frame(efectos = unique((Xcenso$efectos))), 
                 by = "efectos") %>% mutate_all(~replace(., is.na(.), 0)) %>% 
      as.data.frame()

###-------- Anexano

delta = Xencuesta %>% mutate(factorex = encuesta$factorex) %>% group_by(efectos) %>% 
        summarise(deltad = (sum(factorex)^(-2) * sum(factorex^2))) %>%
        right_join(data.frame(efectos = unique(Xcenso$efectos)), 
                   by = "efectos") %>% mutate_all(~replace(., is.na(.), Inf)) %>% 
        as.data.frame()

###-------- Tamaños muestrales --------###

N_d = Xcenso %>% lazy_dt() %>% group_by(efectos) %>% summarise(n = n()) %>% 
  as.data.frame()

##################################
##   Census Empirical Best BHF  ##
##################################

# matriz de medias de las covariables del censo


#head(mean_values)

# 1) Generando modelo mixto Battese Harter Fuller para todas las covariables

s = 81958

Xencuesta %<>% mutate(logIngcorte  = log(encuesta$ingcorte + s))

BHFreg <- lmer(logIngcorte ~ depto05 + depto08 + depto11 + depto13 + depto15 + depto17 + depto18 + 
                 depto19 + depto20 + depto23 + depto25 + depto27 + depto41 + depto44 + depto47 + 
                 depto50 + depto52 + depto54 + depto63 + depto66 + depto68 + depto70 + depto73 + 
                 Area + tipo_viv_casa + tipo_viv_depto + tipo_viv_cuarto + tipo_viv_indigena + 
                 mpared1 + mpared2 + mpared3 + mpared4 + mpared5 + mpared6 + mpisos1 + mpisos2 +
                 mpisos3 + mpisos4 + mpisos5 + electrica_ee + acueducto + alcantarillado + 
                 gasnatural_redp + rec_basura + internet + servhig1 + servhig2 + servhig3 + 
                 servhig4 + servhig5 + tamhog1 + tamhog2 + tamhog3 + hacinamiento + prep_alim1 + 
                 prep_alim2 + prep_alim3 + prep_alim4 + prep_alim5 + agua_alim1 + agua_alim2 + 
                 agua_alim3 + agua_alim4 + agua_alim5 + agua_alim6 + agua_alim7 + agua_alim8 + 
                 jefe_Mujer + prop_mujeres + Edad_jefe2 + Edad_jefe3 + Edad_jefe4 + nhijos_hogar1 +
                 nhijos_hogar2 + nhijos_hogar3 + prop_alfabeta + inasistente + Jefe_sup +
                 ratio_prim + ratio_media + ratio_sup + prop_ocupados + prop_desocupados +
                 prop_inactivos + trabajo_infantil + r_solt + r_casad + migrante_medianop +
                 migrante_cortop + (1|efectos),
               weights = SWeights,
               data = Xencuesta)
#save(BHFreg, file = "4. Modelo SAE censo/Output/ModeloBHF.RData")
## Gráfico cuantil-cuantil de residuos y efectos aleatorios

ud = cbind.data.frame(indice = rownames(ranef(BHFreg)$efectos)
                      ,ud = ranef(BHFreg)$efectos[[1]])
par(mfrow = c(1,2))
qqnorm(ud$ud)
qqline(ud$ud)

qqnorm(residuals(BHFreg))
qqline(residuals(BHFreg))


plot(residuals(BHFreg))
abline(h = 0)

summary(residuals(BHFreg))


#######################################
# Procesamiento modelo SAE montecarlo #
#######################################

# Guardando coeficientes de regresión, varianza de efectos aleatorios y residuos del modelo

betas <- as.matrix(fixed.effects(BHFreg))
var_e <- summary(BHFreg)$sigma^2
var_u <- as.numeric(VarCorr(BHFreg))
gammad <- var_u/(var_u+var_e*delta$deltad)
sum(gammad == 0)
# Tamaños muestrales y número de dominios 
Prov = n_d$efectos
D = dim(n_d)[1]

# Creación vector X^T*Beta y generando matriz \mu de medias condicionales 
setdiff(names(Xcenso),rownames(betas))

XBCenso = data.frame(efectos = Xcenso$efectos, 
                     XB = as.matrix(Xcenso %>% select(rownames(betas)) %>% sapply(as.numeric))%*%betas)
# saveRDS(XBCenso, "4. Modelo SAE censo/Output/XBCenso.rds")


mu <- numeric(dim(Xcenso)[1])
# Seleccionando mean values asociados a los betas de la regresion\

mean_values <- Xencuesta %>% lazy_dt() %>% 
  mutate("(Intercept)" = 1) %>%
  group_by(efectos) %>%
  summarise_at(vars(rownames(betas)), funs(weighted.mean(., w = SWeights))) %>%
  arrange(efectos) %>% as.data.frame() 

mean_values2 <- mean_values %>% select(rownames(betas))

for (i in 1:D){
  print(i)
  provi <- which(Xcenso$efectos == Prov[i])
  if (n_d$n[i] != 0) {
    meany <- weighted.mean(Xencuesta[Xencuesta$efectos == Prov[i],]$logIngcorte, Xencuesta[Xencuesta$efectos == Prov[i],]$SWeights)
    meanx_beta <- as.matrix(mean_values2[which(mean_values$efectos == Prov[i]),] )%*%betas # con intercepto
    add_term <- rep(gammad[i]*(meany - meanx_beta),length(provi))
  }else{
    add_term <- rep(0,length(provi))
  }
  
  mu[provi] = XBCenso$XB[provi]+add_term
}

# Creando matriz de varianzas condicionales


vard <- numeric(dim(Xcenso)[1])

for (i in 1:D){
  print(i)
  provi <- which(Xcenso$efectos == Prov[i])
  if (n_d$n[i]!=0){
    vardi <- var_u*(1-gammad[i]) + var_e
    vard[provi] <- as.matrix(rep(vardi, length(provi)))
  }else{
    vardi <- var_u + var_e
    vard[provi] <- as.matrix(rep(vardi,length(provi)))
  }
}

sd_Census <- sqrt(vard)


####################################
# Totales para función benchmark   #
# Nacional - Urbano - Departamento #
####################################

# Totales de ingreso
medias <- numeric(26)
medias[1] <- encuesta %>% summarise(p = weighted.mean(ingcorte,factorex)) %>% as.numeric()
medias[2] <- encuesta %>% filter(cabecera == 1) %>% summarise(p = weighted.mean(ingcorte,factorex)) %>% as.numeric()
medias[3:26] <- encuesta %>% group_by(DEPARTAMENTO) %>% 
  summarise(p = weighted.mean(ingcorte,factorex)) %>% select(p) %>% unlist(use.names = FALSE)

# Proporciones de pobreza

proporciones <- numeric(26)
proporciones[1] <- encuesta %>% summarise(p = weighted.mean(pobreza,factorex)) %>% as.numeric()
proporciones[2] <- encuesta %>% filter(cabecera == 1) %>% summarise(p = weighted.mean(pobreza,factorex)) %>% as.numeric()
proporciones[3:26] <- encuesta %>% group_by(DEPARTAMENTO) %>% 
  summarise(p = weighted.mean(pobreza,factorex)) %>% select(p) %>% unlist(use.names = FALSE)

# Proporciones de pobreza extrema

proporcionesEx <- numeric(26)
proporcionesEx[1] <- encuesta %>% summarise(p = weighted.mean(pobrezaEx,factorex)) %>% as.numeric()
proporcionesEx[2] <- encuesta %>% filter(areageo2 == 1) %>% summarise(p = weighted.mean(pobrezaEx,factorex)) %>% as.numeric()
proporcionesEx[3:26] <- encuesta %>% group_by(DEPARTAMENTO) %>% 
  summarise(p = weighted.mean(pobrezaEx,factorex)) %>% select(p) %>% unlist(use.names = FALSE)

# Matriz para benchmark
names(Xcenso)
levels(encuesta$DEPARTAMENTO)
MatrizCalibrada <- Xcenso %>% select(area.urbana = Area, starts_with("depto")) %>%
  mutate(depto76 = 1-rowSums(Xcenso[,3:25]),unos = 1) %>% select(unos, area.urbana,everything())

# saveRDS(medias, "4. Modelo SAE censo/Output/medias.rds")
# saveRDS(proporciones, "4. Modelo SAE censo/Output/proporciones.rds") 
# saveRDS(proporcionesEx, "4. Modelo SAE censo/Output/proporcionesEx.rds")
# saveRDS(MatrizCalibrada, "4. Modelo SAE censo/Output/MatrizCalibrada.rds")
# 



############################
## Monte carlo simulation ##
############################
unique(encuesta$li)
unique(encuesta$lp)

lp <- c(296845, 200760)
li <- c(147169, 127346)

source("4. Modelo SAE censo/R codes/0 funcion benchmark.R")

censo_zone0 <- cbind.data.frame(Municipio = Xcenso$Municipio, 
                                Area =  Xcenso$Area)
A = 50

CensusEB <- matrix(0, nrow = 1122,ncol = 7)

for(a in 1:A){
  print(a)
  
  censoY.star <- rnorm(nrow(Xcenso),mu,sd_Census)
  censoYSAE <- exp(censoY.star) - s
  censoYSAE <- ifelse(censoYSAE < 0, 0, censoYSAE)
  censo_zone <- cbind.data.frame(censo_zone0,
                                 censoYSAE) %>% lazy_dt()
  estimates <- censo_zone %>% mutate(
    a.ingreso = Benchmark(censoYSAE),
    indicadorlp = ifelse((Area == 1 &  censoYSAE < lp[1]) | (Area == 0 & censoYSAE < lp[2]), 1, 0),
    indicadorli = ifelse((Area == 1 & censoYSAE < li[1]) | (Area == 0 & censoYSAE < li[2]), 1, 0),
    a.pobreza = Benchmark2(indicadorlp),
    a.pobrezaEx = Benchmark2(indicadorli, extrema = TRUE)) %>% 
    group_by(Municipio) %>%
    summarise(ingcorte.medio = weighted.mean(censoYSAE,a.ingreso),
              Total_P = round(sum(indicadorlp*a.pobreza)),
              Total_I = round(sum(indicadorli*a.pobrezaEx)),
              FGT0p = weighted.mean(indicadorlp,a.pobreza),
              FGT0i = weighted.mean(indicadorli,a.pobrezaEx),
              Qsratio = laeken::qsr(censoYSAE)$value,
              Gini = laeken::gini(censoYSAE)$value
    ) %>% as.data.frame()
  CensusEB <- CensusEB + as.matrix(estimates[,-1])
}

# saveRDS(CensusEB, file = "4. Modelo SAE censo/Output/CensusEB250.rds")

Estimaciones <- as.data.frame((CensusEB)/A)
Estimaciones$Municipio <- estimates$Municipio
Estimaciones %<>% select(Municipio, everything())
saveRDS(Estimaciones, file = "4. Modelo SAE censo/Output/EstimacionesMontecarloSENATEWEIGHTS.rds")
