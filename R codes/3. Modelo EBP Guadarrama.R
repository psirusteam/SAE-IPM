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
library(moments)
select <- dplyr::select

###--- Definiendo la memoria RAM a emplear ---###

memory.limit(18000000)

###---------------------------- Encuesta original ---------------------------###

encuesta <- readRDS("Input/1. Data/GEIH2018.rds") %>% 
            mutate(idhogar = paste0(directorio, secuencia_p), 
                   pobrezaEx = ifelse(pobreza == 1, 1, 0), 
                   pobreza = ifelse(pobreza != 3, 1, 0), estrato = paste(mes, 
                   DEPARTAMENTO, divipola, clase.x, sep = "_" ), 
                   upm =  paste(estrato, segmento, sep = "_" ),
                   Priv_Ing = ifelse(yemp < lp, 1, 0),
                   PEA = ifelse(sexo == 1 & edad %in% 18:65, 1,
                          ifelse(sexo == 2 & edad %in% 18:60, 1, 0)),
                   Area = ifelse(clase.x == 1, 1, 0),
                   efectos = paste0(divipola, "-", as.character(Area)))

###------------ Filtro sobre la población económicamente activa -------------###

encuesta <- subset(encuesta, PEA == 1)
encuesta$SWeights <- nrow(encuesta)*(encuesta$factorex/sum(encuesta$factorex))

###------------------ Seleccionando algunas variables: Censo ----------------###

Xcenso <- readRDS("Input/1. Data/Xcenso.rds") %>% mutate("(Intercept)" = 1,
          Municipio = ifelse(as.numeric(as.character(Municipio)) < 10000, 
          paste0(0, as.character(Municipio)), as.character(Municipio)),
          efectos = paste0(Municipio, "-", as.character(Area))) 

################################################################################
###------------------- Tamaños muestrales y poblacionales -------------------###
###-------------------     Dominios: Municipio - área     -------------------###
################################################################################

###--------------------------- Tamaños muestrales ---------------------------###

n_d = encuesta %>% group_by(efectos) %>% summarise(n = n()) %>%
      right_join(data.frame(efectos = unique((Xcenso$efectos))), 
                 by = "efectos") %>% mutate_all(~replace(., is.na(.), 0)) %>% 
      as.data.frame()

###---------------------------- Estimación delta ----------------------------###

delta = encuesta %>% mutate(factorex = factorex) %>% group_by(efectos) %>% 
        summarise(deltad = (sum(factorex)^(-2) * sum(factorex^2))) %>%
        right_join(data.frame(efectos = unique(Xcenso$efectos)), 
                   by = "efectos") %>% mutate_all(~replace(., is.na(.), Inf)) %>% 
        as.data.frame()

###-------------------------- Tamaños Poblacionales -------------------------###

N_d = Xcenso %>% lazy_dt() %>% group_by(efectos) %>% summarise(n = n()) %>% 
                 as.data.frame()

################################################################################
###-------------- Identificando la constante de transformación --------------###
################################################################################

# constante <- lapply(X = seq(100, 1000000, 100), FUN = function(y){
#                     logIngcorte = log(encuesta$yemp + y)
#                     simetria <- abs(skewness(logIngcorte))
#                     data.frame(y, simetria)
#                     }) %>% bind_rows()
# 
# s = constante[which.min(constante$simetria), 1]

s = 104300

###--- Creando la variable respuesta ---###

encuesta %<>% mutate(logIngcorte  = log(encuesta$yemp + s))

################################################################################
###------------------------ Census Empirical Best BHF -----------------------###
################################################################################

# P5020: Tipo de servicio sanitario                                            #
# P5040: ¿cómo eliminan principalmente la basura en este hogar?                #
# P5050: ¿de dónde obtiene este hogar el agua para consumo humano?             #
# P5080: ¿con qué energía o combustible cocinan principalmente en este hogar?  #
# P5090: La vivienda ocupada por este hogar es propia?                         #
# P4000: Tipo de vivienda                                                      #
# P4010: ¿cuál es el material predominante de las paredes de la vivienda?      #
# P4020: ¿cuál es el material predominante de los pisos de la vivienda?        #
# P4030S1: Energía eléctrica                                                   #
# P4030S2: Gas natural conectado a red pública                                 #
# P4030S3: Alcantarillado                                                      #
# P4030S4: Recolección de basuras                                              #
# P4030S5: Acueducto                                                           #
# P5210s3: Internet                                                            #
# P6008: Total de personas en el hogar                                         #
# P6160: Sabe leer y escribir                                                  #
# P6070: Estado civil                                                          #
# P6210: Nivel educativo                                                       #
# P755s1, s3                                                                   #

# BHFreg <- lmer(logIngcorte ~ DEPARTAMENTO + Area + P5020 + P5040 +
#                  internet + servhig1 + servhig2 + servhig3 + servhig4 + 
#                  servhig5 + tamhog1 + tamhog2 + tamhog3 + hacinamiento + 
#                  jefe_Mujer + prop_mujeres + Edad_jefe2 + Edad_jefe3 + 
#                  Edad_jefe4 + nhijos_hogar1 + nhijos_hogar2 + nhijos_hogar3 + 
#                  prop_alfabeta + inasistente + Jefe_sup + prop_ocupados + 
#                  prop_desocupados + prop_inactivos + trabajo_infantil +  
#                  migrante_medianop +  migrante_cortop + (1|efectos),
#                weights = SWeights,
#                data = encuesta)

BHFreg <- lmer(logIngcorte ~ DEPARTAMENTO + Area + p5020 + p5040 + p5050 +
               p5080 + p5090 + p4000 + p4010 + p4020 + p4030s1 + p4030s2 + 
               p4030s3 + p4030s4 + p4030s5 + p5210s3 + p6160 + p6070 + p6210 +
               p6008 + nhijos + sexoj + edadj + ocupr_p + (1|efectos),
               weights = SWeights, data = encuesta)

save(BHFreg, file = "Output/ModeloBHF.RData")

load("Output/ModeloBHF.RData")

###-------------- Extrayendo los efectos aleatorios del modelo --------------###

ud = cbind.data.frame(indice = rownames(ranef(BHFreg)$efectos),
                      ud = ranef(BHFreg)$efectos[[1]])

###--------------------------------------------------------------------------###
###----------- Normalidad de los residuos y los efectos aleatorios ----------###
###--------------------------------------------------------------------------###


###--- Gráfico cuantil-cuantil de residuos y efectos aleatorios ---###

par(mfrow = c(1,2))
qqnorm(ud$ud)
qqline(ud$ud)
qqnorm(residuals(BHFreg))
qqline(residuals(BHFreg))

windows()
plot(residuals(BHFreg))
abline(h = 0)

sum()

summary(residuals(BHFreg))


#######################################
# Procesamiento modelo SAE montecarlo #
#######################################

# Guardando coeficientes de regresión, varianza de efectos aleatorios y residuos del modelo

betas <- as.matrix(fixed.effects(BHFreg))
var_e <- summary(BHFreg)$sigma^2
var_u <- as.numeric(VarCorr(BHFreg))
gammad <- var_u/(var_u + var_e * delta$deltad)
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
medias[1] <- encuesta %>% summarise(p = weighted.mean(ingcorte, factorex)) %>% as.numeric()
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
    indicadorlp = ifelse((Area == 1 & censoYSAE < lp[1]) | (Area == 0 & censoYSAE < lp[2]), 1, 0),
    indicadorli = ifelse((Area == 1 & censoYSAE < li[1]) | (Area == 0 & censoYSAE < li[2]), 1, 0),
    a.pobreza = Benchmark2(indicadorlp),
    a.pobrezaEx = Benchmark2(indicadorli, extrema = TRUE)) %>% 
    group_by(Municipio) %>%
    summarise(ingcorte.medio = weighted.mean(censoYSAE,a.ingreso),
              Total_P = round(sum(indicadorlp * a.pobreza)),
              Total_I = round(sum(indicadorli * a.pobrezaEx)),
              FGT0p = weighted.mean(indicadorlp, a.pobreza),
              FGT0i = weighted.mean(indicadorli, a.pobrezaEx),
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
