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
library(ggpubr)
library(normtest)
library(rgdal)
library(sp)
library(RColorBrewer)
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

Xencuesta <-  readRDS("Input/1. Data/Xencuesta.rds") 

encuesta <- left_join(encuesta, Xencuesta)

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

###------ Función para identificar el valor para transformar el ingreso -----### 

# constante <- lapply(X = seq(100, 100000, 1000), FUN = function(y){
#                     logIngcorte = log(encuesta$yemp + y)
#                     ajb <- ajb.norm.test(logIngcorte, nrepl = 2000)
#                     simetria <- skewness.norm.test(logIngcorte, nrepl = 2000)
#                     data.frame(y, ajb$p.value, simetria$p.value)
#                     }) %>% bind_rows()
# 
# s = constante[which.max(constante$ajb), 1]

s = 114300

###--- Creando la variable respuesta ---###

encuesta %<>% mutate(logIngcorte  = log(encuesta$yemp + s))

###--------------------------------------------------------------------------###
###---------- Validación de la normalidad de la variable respuesta ----------###
###--------------------------------------------------------------------------###

###-------- Gráfico cuantil-cuantil de residuos y efectos aleatorios --------###

plot(density(encuesta$logIngcorte))
ggqqplot(encuesta$logIngcorte)

###------------ Pruebas de normalidad sobre variable transformada -----------###

###--- Prueba de Jarque - Bera ajustada ---###

ajb <- ajb.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de Frosini ---###

frosini <- frosini.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de Geary ---###

geary <- geary.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de Hegazy-Green ---###

hegazy1 <- hegazy1.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de Hegazy-Green ---###

hegazy2 <- hegazy2.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de Weisberg-Bingham ---###

wb <- wb.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de Spiegelhalter ---###

sh <- spiegelhalter.norm.test(encuesta$logIngcorte, nrepl = 2000)

###--- Prueba de simetría Shapiro, Wilk and Chen ---###

sime <- skewness.norm.test(encuesta$logIngcorte)

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

BHFreg <- lmer(logIngcorte ~ depto05 + depto08 + depto11 + depto13 + depto15 + 
                   depto17 + depto18 + depto19 + depto20 + depto23 + depto25 + 
                   depto27 + depto41 + depto44 + depto47 + depto50 + depto52 + 
                   depto54 + depto63 + depto66 + depto68 + depto70 + depto73 + 
                   Area + tipo_viv_casa + tipo_viv_depto + tipo_viv_cuarto + 
                   tipo_viv_indigena + mpared1 + mpared2 + mpared3 + mpared4 + 
                   mpared5 + mpared6 + mpisos1 + mpisos2 + mpisos3 + mpisos4 + 
                   mpisos5 + electrica_ee + acueducto + alcantarillado + 
                   gasnatural_redp + rec_basura + internet + servhig1 + 
                   servhig2 + servhig3 + servhig4 + servhig5 + tamhog1 + 
                   tamhog2 + tamhog3 + hacinamiento + prep_alim1 + prep_alim2 + 
                   prep_alim3 + prep_alim4 + prep_alim5 + agua_alim1 + agua_alim2 + 
                   agua_alim3 + agua_alim4 + agua_alim5 + agua_alim6 + agua_alim7 + 
                   agua_alim8 + jefe_Mujer + prop_mujeres + Edad_jefe2 + 
                   Edad_jefe3 + Edad_jefe4 + nhijos_hogar1 + nhijos_hogar2 + 
                   nhijos_hogar3 + prop_alfabeta + inasistente + Jefe_sup +
                   ratio_prim + ratio_media + ratio_sup + prop_ocupados + 
                   prop_desocupados + prop_inactivos + trabajo_infantil + 
                   r_solt + r_casad + migrante_medianop + migrante_cortop + 
                  (1|efectos), weights = SWeights, data = encuesta)

save(BHFreg, file = "Output/ModeloBHF.RData")

#load("Output/ModeloBHF.RData")

###-------------- Extrayendo los efectos aleatorios del modelo --------------###

ud = cbind.data.frame(indice = rownames(ranef(BHFreg)$efectos),
                      ud = ranef(BHFreg)$efectos[[1]])

###--------------------------------------------------------------------------###
###----------- Normalidad de los residuos y los efectos aleatorios ----------###
###--------------------------------------------------------------------------###

###-------- Gráfico cuantil-cuantil de residuos y efectos aleatorios --------###

###--- Efectos aleatorios ---###

ggqqplot(ud$ud)

###--- Residuales ---###

ggqqplot(residuals(BHFreg))

###--------------------------------------------------------------------------###
###------------- Prueba de normalidad sobre el efecto aleatorio -------------###
###--------------------------------------------------------------------------###

plot(ud$ud)
abline(h = 0)

###--- Prueba de Jarque - Bera ajustada ---###

ajb.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de Frosini ---###

frosini.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de Geary ---###

geary.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de Hegazy-Green ---###

hegazy1.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de Hegazy-Green ---###

hegazy2.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de Weisberg-Bingham ---###

wb.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de Spiegelhalter ---###

spiegelhalter.norm.test(ud$ud, nrepl = 2000)

###--- Prueba de simetría Shapiro, Wilk and Chen ---###

skewness.norm.test(ud$ud, nrepl = 2000)

###--------------------------------------------------------------------------###
###----------------- Prueba de normalidad sobre los residuos ----------------###
###--------------------------------------------------------------------------###

plot(residuals(BHFreg))
abline(h = 0)

###--- Prueba de Jarque - Bera ajustada ---###

ajb.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de Frosini ---###

frosini.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de Geary ---###

geary.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de Hegazy-Green ---###

hegazy1.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de Hegazy-Green ---###

hegazy2.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de Weisberg-Bingham ---###

wb.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de Spiegelhalter ---###

spiegelhalter.norm.test(residuals(BHFreg), nrepl = 2000)

###--- Prueba de simetría Shapiro, Wilk and Chen ---###

skewness.norm.test(residuals(BHFreg), nrepl = 2000)

################################################################################
###------------------- Procesamiento modelo SAE Montecarlo ------------------###
################################################################################

###--- Efectos aleatorios ---###

betas <- as.matrix(fixed.effects(BHFreg))

###--- Varianza del término de error ---###

var_e <- summary(BHFreg)$sigma^2

###--- Varianza del efecto aleatorio ---###

var_u <- as.numeric(VarCorr(BHFreg))

###--- Gamma por dominio ---###

gammad <- var_u/(var_u + var_e * delta$deltad)
sum(gammad == 0)

###--- Tamaños muestrales ---###

Prov = n_d$efectos

###--- número de dominios ---###

D = dim(n_d)[1]

###--- Identificando el nombre de las variables ---###

setdiff(names(Xcenso), rownames(betas))

###----------------------- Creación del vector X'Beta -----------------------###

XBCenso = data.frame(efectos = Xcenso$efectos, XB = as.matrix(Xcenso %>% 
                     select(rownames(betas)) %>% sapply(as.numeric)) %*% betas)

saveRDS(XBCenso, "Output/XBCenso.rds")

###---generando matriz \mu de medias condicionales -###

mu <- numeric(dim(Xcenso)[1])

# Seleccionando mean values asociados a los betas de la regresion\

mean_values <- encuesta %>% lazy_dt() %>% 
  mutate("(Intercept)" = 1) %>%
  group_by(efectos) %>%
  summarise_at(vars(rownames(betas)), funs(weighted.mean(., w = SWeights))) %>%
  arrange(efectos) %>% as.data.frame() 

mean_values2 <- mean_values %>% select(rownames(betas))

for (i in 1:D){
  print(i)
  provi <- which(Xcenso$efectos == Prov[i])
  if (n_d$n[i] != 0) {
    meany <- weighted.mean(encuesta[encuesta$efectos == Prov[i],]$logIngcorte, encuesta[encuesta$efectos == Prov[i],]$SWeights)
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
    vardi <- var_u * (1-gammad[i]) + var_e
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
  mutate(depto76 = 1 - rowSums(Xcenso[,3:25]),unos = 1) %>% select(unos, area.urbana,everything())

# saveRDS(medias, "Output/medias.rds")
# saveRDS(proporciones, "Output/proporciones.rds") 
# saveRDS(proporcionesEx, "Output/proporcionesEx.rds")
# saveRDS(MatrizCalibrada, "Output/MatrizCalibrada.rds")
 

############################
## Monte carlo simulation ##
############################

unique(encuesta$li)
unique(encuesta$lp)

lp <- c(296845, 200760)
li <- c(147169, 127346)

source("R codes/0. Benchmark.R")

censo_zone0 <- cbind.data.frame(Municipio = Xcenso$Municipio, 
                                Area =  Xcenso$Area)
A = 50

CensusEB <- matrix(0, nrow = 1122, ncol = 7)

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

saveRDS(CensusEB, file = "Output/CensusEB250.rds")

Estimaciones <- as.data.frame((CensusEB)/A)
Estimaciones$Municipio <- estimates$Municipio
Estimaciones %<>% select(Municipio, everything())
Estimaciones$FGT0p <- abs(Estimaciones$FGT0p)
Estimaciones$FGT0i <- abs(Estimaciones$FGT0i)
saveRDS(Estimaciones, file = "Output/EstimacionesMontecarloSENATEWEIGHTS.rds")

################################################################################
###---------------------------------- Mapas ---------------------------------###
################################################################################

###--- Cargue del shape ---###

colMun <- readOGR("Input/2. MGN2020_MPIO_POLITICO", "MGN_MPIO_POLITICO")

###--- Anexando estimaciones y pronósticos al shape ---###

EstimMun <- merge(colMun, Estimaciones, by.x = "MPIO_CDPMP",
                  by.y = "Municipio")

###--- Paleta de colores ---###

paleta <- brewer.pal(n = 11, name = "RdYlGn")

###--- Cortes para el mapas del pronóstico del modelo FH ---###

breaksFH_p <- classInt::classIntervals(EstimMun$FGT0p, n = 11, style = "kmeans",
                                       intervalClosure = "right")

breaksFH_i <- classInt::classIntervals(EstimMun$FGT0i, n = 11, style = "kmeans",
                                       intervalClosure = "right")

###--- Mapa de pobreza ---###

windows()
spplot(EstimMun, "FGT0p", col.regions = rev(paleta), at = breaksFH_p$brks)

###--- Mapa de indigencia---###

windows()
spplot(EstimMun, "FGT0i", col.regions = rev(paleta), at = breaksFH_i$brks)