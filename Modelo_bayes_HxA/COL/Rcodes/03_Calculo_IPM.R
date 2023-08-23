################################################################################
## Proyecto del IPM = HxA 
## Andrés Gutiérrez y Stalyn Guerrero 
################################################################################

################################################################################
## Librerias 
################################################################################
rm(list = ls())
library(tidyverse)
library(rstantools)
library(rstan)
library(posterior)
library(patchwork)
library(lme4)
library(rstanarm)
library(magrittr)
library(furrr)
library(openxlsx)
source("Modelo_bayes_HxA/0funciones/Estimar_ipm.R")
source("Modelo_bayes_HxA/0funciones/agregado_dim_ipm.r")
################################################################################
# Lectura de base de datos
################################################################################
censo_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

################################################################################
## Lectura de variables dummy  
################################################################################

epred_mat_agua_dummy <-  readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_agua_dummy.rds")
epred_mat_educacion_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_educacion_dummy.rds")
epred_mat_empleo_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_empleo_dummy.rds")
epred_mat_energia_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_energia_dummy.rds")
epred_mat_hacinamiento_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_hacinamiento_dummy.rds")
epred_mat_internet_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_internet_dummy.rds")
epred_mat_material_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_material_dummy.rds")
epred_mat_saneamiento_dummy <- readRDS("Modelo_bayes_HxA/COL/Data/epred_mat_saneamiento_dummy.rds")

chain_q  <- 0.1 * (
  epred_mat_material_dummy +
    epred_mat_hacinamiento_dummy +
    epred_mat_agua_dummy +
    epred_mat_saneamiento_dummy +
    epred_mat_energia_dummy + epred_mat_internet_dummy
) +
  0.2 * (epred_mat_educacion_dummy +
           epred_mat_empleo_dummy)

# saveRDS(chain_q, "Modelo_bayes_HxA/COL/Data/chain_q.rds")

chain_Indicadora <- chain_q

chain_Indicadora[chain_Indicadora <= 0.4] <- 0
chain_Indicadora[chain_Indicadora != 0] <- 1


D <- rowSums(chain_Indicadora*censo_ipm$n)
Q <- rowSums(chain_q*censo_ipm$n)
chain_H =  D/sum(censo_ipm$n)
chain_A = Q/((sum(censo_ipm$n))*8)

chain_IPM <- (D*Q)/((sum(censo_ipm$n)^2)*8)

mean(chain_H)
sd(chain_H)

mean(chain_A)
sd(chain_A)

mean(chain_IPM)
sd(chain_IPM)

estime_IPM(poststrat = censo_ipm,
           chain_q,
           byMap = NULL, 
           n_dim = 8) %>% data.frame()

estime_IPM(poststrat = censo_ipm,
           chain_q,
           byMap = "dam", 
           n_dim = 8) %>% data.frame()

estime_IPM(poststrat = censo_ipm,
           chain_q,
           byMap = "dam2", 
           n_dim = 8) %>% data.frame()

###################################################
## Resultado por dimensiones ######################
###################################################
epred_mat_dim <- list(
  Material = epred_mat_material_dummy,
  Hacinamienot =    epred_mat_hacinamiento_dummy ,
  Agua =  epred_mat_agua_dummy, 
  Saneamiento =  epred_mat_saneamiento_dummy, 
  Energia = epred_mat_energia_dummy ,
  Internet = epred_mat_internet_dummy,
  Educacion = epred_mat_educacion_dummy , 
  Empleo =  epred_mat_empleo_dummy)

aux_agregado <- function(dat, byx = NULL, censo) {
  temp_estimate <- map_df(dat,
                          function(dummy) {
                            agregado_dim_ipm(poststrat = censo,
                                             epredmat = dummy,
                                             byMap = byx)
                          }, .id = "Indicador")
  
  inner_join(
    spread(
      temp_estimate %>% select(-estimate_se),
      key = "Indicador",
      value = "estimate"
    ),
    spread(
      temp_estimate %>% select(-estimate),
      key = "Indicador",
      value = "estimate_se"
    ) %>%
      rename_if(
        is.numeric,
        .funs = function(x)
          paste0(x, "_se")
      )
  )
  
}

#############################################################

by_agrega  <- c("dam", "dam2",  "area",
                "sexo",  "edad",  "etnia",
                "anoest")

estimado_ipm1 <-map(by_agrega, function(xby){
  
paso_ipm <- estime_IPM(poststrat = censo_ipm,
           chain_q,
           byMap = xby, 
           n_dim = 8) %>% data.frame()

paso_dim <- aux_agregado(epred_mat_dim, xby, censo_ipm) %>% data.frame()

inner_join(paso_dim,paso_ipm)

})

names(estimado_ipm1) <- by_agrega


by_agrega2 <- t(combn(by_agrega[-c(1:2)], 2)) %>% cbind("dam")

estimado_ipm2 <- map(1:nrow(by_agrega2), function(ii) {
  paso_ipm <- estime_IPM(poststrat = censo_ipm,
                         chain_q,
                         byMap = by_agrega2[ii, ],
                         n_dim = 8) %>% data.frame()
  
  paso_dim <-
    aux_agregado(epred_mat_dim, by_agrega2[ii, ], censo_ipm) %>% data.frame()
  
  inner_join(paso_dim, paso_ipm)
})

names(estimado_ipm2) <- apply(by_agrega2, 1, paste0, collapse = "_")

estimado_ipm <- c(estimado_ipm1, estimado_ipm2) 
saveRDS(estimado_ipm, file = "Modelo_bayes_HxA/COL/Data/estimado_ipm.rds")

## creando libro de excel. 
wb <- createWorkbook()
hojas <- names(estimado_ipm)
## Creando la hoja del índice. 
addWorksheet(wb, "Indice")
## Creando la tablas de índice 
datos <- data.frame(Orden = 1:length(hojas),
                    Titulo = NA)
## agregando el indice al libro de excel
writeDataTable(
  wb = wb,
  x = datos,
  sheet = "Indice",
  rowNames = FALSE,
  tableStyle = "TableStyleLight9"
)

## Agregando los resultados al libro de excel hoja por hoja
for(ii in 1:length(hojas)) {
  addWorksheet(wb, hojas[ii])
  writeData(
    wb = wb,
    x = estimado_ipm[[hojas[ii]]],
    sheet = hojas[ii],
    rowNames = FALSE
  )
  ## agregando el nombre de la hoja al índice
  writeFormula(
    wb,
    "Indice",
    startRow = 1 + ii,
    startCol = 2,
    x = makeHyperlinkString(
      sheet = hojas[ii],
      row = 1,
      col = 2,
      text = hojas[ii]
    )
  )
  
}

saveWorkbook(wb, file = "Modelo_bayes_HxA/COL/Output/estimacion_ipm.xlsx",
             overwrite = TRUE)
openxlsx::openXL("Modelo_bayes_HxA/COL/Output/estimacion_ipm.xlsx")

############ Estimaciones por dimension del IPM #####################

temp_epred_mat <- list(
  Material = epred_mat_material_dummy,
  Hacinamienot =    epred_mat_hacinamiento_dummy ,
  Agua =  epred_mat_agua_dummy, 
  Saneamiento =  epred_mat_saneamiento_dummy, 
  Energia = epred_mat_energia_dummy ,
  Internet = epred_mat_internet_dummy,
  Educacion = epred_mat_educacion_dummy , 
  Empleo =  epred_mat_empleo_dummy)



temp_estimate_mpio <- map_df(temp_epred_mat,
                             function(dummy) {
                               agregado_dim_ipm(poststrat = censo_ipm,
                                                epredmat = dummy,
                                                byMap = "dam2") 
                             }, .id = "Indicador")

saveRDS(temp_estimate_mpio, "Modelo_bayes_HxA/COL/data/temp_estimate_mpio.rds")


