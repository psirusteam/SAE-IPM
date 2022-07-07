#########################################################
# Proyecto MRP - Left No One Behind                     #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###
rm(list = ls())
cat("\f")

library(Rcpp)
library(RcppProgress)
library(redatam)
library(dplyr)
library(tidyverse)
library(haven)
library(DataExplorer)
library(magrittr)

## leer base desde el repositorio CEPAL
colombia <- redatam.open("Frecuentista_depto/COL/data/cpv2018col-cde.dicX")
redatam.entities(colombia)
redatam.variables(colombia, entName =  "MUPIO")
redatam.variables(colombia, entName = "VIVIENDA")
redatam.variables(colombia, entName = "PERSONA")

CONTEOS <- redatam.query(colombia,
                      "freq DEPTO.REDCODEN
                      by VIVIENDA.ipm_Material
                      by VIVIENDA.ipm_Saneamiento
                      by VIVIENDA.ipm_Energia
                      by VIVIENDA.ipm_Internet
                      by HOGAR.ipm_Agua
                      by HOGAR.ipm_Hacinamiento
                      by PERSONA.EDAD5
                      by CLASE.AREA
                      by PERSONA.P_SEXO
                      ",
                      tot.omit = FALSE)
saveRDS(CONTEOS, file = "Frecuentista_depto/COL/Data/CONTEOS2.rds")
rm(list = "$table1")
#CONTEOS <-  readRDS(file = "Frecuentista_depto/COL/Data/CONTEOS2.rds")

#   revisando valores unicos.
map(grep(pattern = "_value", x = names(CONTEOS),value = TRUE),
    function(by){
      unique(CONTEOS[[by]])
    })
# Eliminando totales de la tabla
CONTEOS2 <- CONTEOS %>%
  filter_at(vars(matches("_label")),all_vars(. !=  "__tot__"))

## sumas por variables de agregación, coincidir con el total nacional.
map(grep(pattern = "_value", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })

map(grep(pattern = "_label", x = names(CONTEOS2),value = TRUE),
    function(by){
      CONTEOS2 %>% group_by_at(by) %>%
        summarise(n = sum(value)) %>%
        mutate(Prop = n / sum(n), N = sum(n))
    })


# Se filtra para mayores de 4 años por las variables NAS en idioma
censo_mrp <- CONTEOS2 %>%
  transmute(depto =str_pad(string = REDCODEN1_value, width = 2, pad = "0"),
            ipm_Material = ipm_Material2_value,
            ipm_Saneamiento = ipm_Saneamiento3_value,
            ipm_Energia = ipm_Energia4_value,
            ipm_Internet = ipm_Internet5_value,
            ipm_Agua = ipm_Agua6_value, 
            ipm_Hacinamiento = ipm_Hacinamiento7_value,
            edad = EDAD58_label,
            area = case_when(AREA9_value == 1 ~ "1",
                             TRUE ~ "0"),
            sexo = P_SEXO10_value,
            #etnia = PBLOPER11_value,
            #anoest = EDUCA212_value,
            value) %>%
  filter(edad !=1  ) %>% 
  group_by_at(
    vars(depto, sexo:edad, ipm_Material:ipm_Hacinamiento)) %>%
  summarise(n = sum(value), .groups = "drop") 

# Suma del total nacional
sum(censo_mrp$n)

saveRDS(censo_mrp, "Frecuentista_depto/COL/Data/censo_ipm2.rds")
