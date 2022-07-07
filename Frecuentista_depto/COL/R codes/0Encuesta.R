#########################################################
# Proyecto IPM                                          #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero & Andrés Gutiérrez             #
#########################################################

### Cleaning R environment ###

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
#library(StatisticalModels)
library(fastDummies)
library(haven)
library(magrittr)
library(stringr)
library(openxlsx)
select <- dplyr::select

###------------ Definiendo el límite de la memoria RAM a emplear ------------###

memory.limit(250000000)

################################################################################
###----------------------------- Loading datasets ---------------------------###
################################################################################
## Leer encuesta
# encuesta_temp <- read_dta("Z:/BC/COL_2018N1.dta")
# 
# encuesta <- read_dta("Z:/BG/col18n1/col18n1.dta")
# encuesta %<>% mutate(orden_temp = str_pad(
#   string = 1:n(),
#   width = 7,
#   pad = "0"
# ))
# 
# upms <- readRDS(file = "Frecuentista_depto/COL/Data/upm_dpto_2018.rds")
# 
# encuesta %<>% left_join(upms,
#                         by = c("directorio" = "DIRECTORIO",
#                                "secuencia_p" = "SECUENCIA_P",
#                                "orden"))
# 
# encuesta$mpio <- substr(encuesta$segmento,8,12)
# 
# encuesta %<>% arrange(orden_temp)
# encuesta$condactr_ee <- encuesta_temp$condactr_ee
# encuesta$etnia_ee <- encuesta_temp$etnia_ee
# encuesta$fep <- encuesta_temp[["_fep"]]
# table(encuesta$condact3, useNA = "a")
# encuesta %<>% filter(edad >= 18)
# saveRDS(encuesta, "Frecuentista_depto/COL/Data/encuesta2018.rds")
encuesta <- readRDS("Frecuentista_depto/COL/Data/encuesta2018.rds")
################################################################################
### GEIH: Creando las dimensiones de interés ###
################################################################################

# Materiales --------------------------------------------------------------
# P4010 Material de las paredes 
# p4020 Material del piso 
prop.table( table(encuesta$p4010))
prop.table(table(encuesta$p4020, useNA = "a"))

encuesta %<>% mutate(ipm_Material_pared = ifelse(p4010 %in% c(1,2),0,1), 
                     ipm_Material_piso =  ifelse(p4020 %in% c(4,5,6,7),0,1),
                     ipm_Material = ifelse(ipm_Material_piso == 0 & 
                                             ipm_Material_pared == 0, 0,1)
                     )

prop.table(table(encuesta$ipm_Material_pared,encuesta$ipm_Material_piso))

prop.table(table(encuesta$ipm_Material, useNA = "a"))


# Hacinamiento  -----------------------------------------------------------

# P6008: Numero de personas en el Hogar 
# P5010: Número de cuartos (dormitorios)

encuesta %<>% mutate(ipm_Hacinamiento = ifelse(p6008/p5010 > 2, 1,0 ))
table(encuesta$ipm_Hacinamiento, useNA = "a")



# Agua --------------------------------------------------------------------
# p5050: ¿de dónde obtiene principalmente este hogar el agua para
# consumo humano? 
# areageo: zona Urbana y Rura
encuesta$areageo
encuesta %<>% mutate(ipm_Agua = case_when(
  areageo2 == 1 & !p5050 %in% 1 ~ 1,
  areageo2 == 2 & !p5050 %in% c(1,2) ~ 1,
  TRUE ~ 0))

table(encuesta$ipm_Agua)


# Saneamiento -------------------------------------------------------------
# areageo: zona Urbana y Rura
# p5020 : El servicio sanitario que utiliza el hogar es:
# p5030 : Uso compartido de sanitario 
encuesta$areageo
encuesta %<>% mutate(ipm_Saneamiento = case_when(
  areageo2 == 1 & !p5020 %in% c(1,2)   ~ 1,
  areageo2 == 2 & !p5020 %in% c(1,2)   ~ 1,
  TRUE ~ 0))

table(encuesta$ipm_Saneamiento)


# Energía  ----------------------------------------------------------------
# p5080 Cocina 
# p4030S1 Energía 
encuesta %<>% mutate(ipm_Energia = case_when(
  !p5080 %in% c(1,3) | p4030s1 == 2 ~1,
  TRUE ~ 0))
table(encuesta$ipm_Energia, useNA = "a")


# Internet ----------------------------------------------------------------
# P5210S3 Internet
encuesta %<>% mutate(ipm_Internet = case_when(
  p5210s3 == 2 ~1,
  TRUE ~ 0))
table(encuesta$ipm_Internet, encuesta$p5210s3, useNA = "a")


# Educación ---------------------------------------------------------------
table(encuesta$p6210s1)
# p6170: ¿actualmente ... Asiste a la escuela, colegio o universidad? 

# p6210s1 : ¿Cuál es el nivel educativo más alto alcanzado por .... y el último año o grado
# aprobado en este nivel? Grado 

encuesta %<>% mutate(
  ipm_educacion = case_when( edad < 20 & p6170 == 2 & p6210s1 < 11 ~  1,
                             edad < 30 & p6210s1 < 11 ~ 1, 
                             edad < 60 & p6210s1 < 9 ~1, 
                             edad >=60 & p6210s1 < 4 ~ 1,
                             TRUE ~ 0))

prop.table(table(encuesta$ipm_educacion, useNA = "a"))
table(encuesta$p6210s1, useNA = "a")


###--------------- Aseguramiento Salud y formalidad laboral -----------------###

### p6090. ¿ ... está afiliado, es cotizante o es  beneficiario de alguna    ### 
### entidad de seguridad social en salud? (Instituto de Seguros Sociales     ###
### - ISS, Empresa Promotora de Salud - EPS o Administradora de Régimen      ###
### Subsidiado - ARS) 1 Sí 2 No 9 No sabe, no informa                        ###

### p6920: ¿Está... cotizando actualmente a un fondo de pensiones? 1 Sí 2 No ###
###        3 Ya es pensionado                                                ###

# Privación del empleo 
# desempleado (condact3 == 2)
# fuera de la fuerza laboral por quehacer domestico (condactr_ee == 4)
# Ocupación con ingreso < lp (condact3 == 1 & yemp < lp)
# sin aporte a seguridad social (p6920 == 2)
table(encuesta$sexo)

encuesta %<>% mutate(
  idhogar = paste0(directorio, secuencia_p),
  ipm_Empleo_Aseguramiento = case_when( 
    sexo == 2 & edad < 60 & condact3 == 2 ~ 1, 
    sexo == 2 & edad < 60 & condactr_ee == 4 ~1,
    sexo == 2 & edad < 60 & condact3 == 1 & yemp <lp ~1,
    sexo == 2 & edad < 60 & p6920 == 2 ~1,
    sexo == 1 & edad < 65 & condact3 == 2 ~ 1, 
    sexo == 1 & edad < 65 & condactr_ee == 4 ~1,
    sexo == 1 & edad < 65 & condact3 == 1 & yemp <lp ~1,
    sexo == 1 & edad < 65 &  p6920 == 2 ~1,
    sexo == 2 & edad >= 60 & p6920 != 3 ~1,
    sexo == 1 & edad >= 65 & p6920 != 3 ~1,
    sexo == 2 & edad >= 60 & p6920 != 1 & yjub < lp ~1,
    sexo == 1 & edad >= 65 & p6920 != 1 & yjub < lp ~1,
    TRUE ~ 0))

table(encuesta$ipm_Empleo_Aseguramiento, useNA = "a")


### GEIH: Creating the post-stratification variables: Age and Schooling ###

encuesta_ipm <- encuesta %>%
  transmute(
    depto = str_sub(string = mpio,start = 1,end = 2 ),
    mpio,
    
    ingreso = ingcorte, lp, li,
    ipm_Material, ipm_Hacinamiento, ipm_Agua,
    ipm_Saneamiento, ipm_educacion, ipm_Energia,
    ipm_Internet, ipm_Empleo_Aseguramiento,
    area = case_when(areageo2 == 1 ~ "1",
                     TRUE ~ "0"),
    sexo = as.character(sexo),
    
    etnia = case_when(
      etnia_ee == 1 ~ "1", # Indígena
      etnia_ee == 2 ~ "2", # Afro
      TRUE ~ "3"), # Otro
    
    anoest = case_when(
      edad < 5 | is.na(anoest)   ~ "98"  , #No aplica
      anoest == 99 ~ "99", #NS/NR
      anoest == 0  ~ "1", # Sin educacion
      anoest %in% c(1:6) ~ "2",       # 1 - 6
      anoest %in% c(7:12) ~ "3",      # 7 - 12
      anoest > 12 ~ "4",      # mas de 12
      TRUE ~ "Error"  ),
    
    
    edad = case_when(
      edad <= 17 ~ NA_character_,
      edad < 30 ~ "2",
      edad < 45 ~ "3",
      edad < 65 ~ "4",
      edad >= 65 ~ "5"),
    condact3 = case_when(condact3 == -1 ~ NA_real_, 
                         TRUE ~ condact3) ,
    fep
  ) %>% filter(!is.na(edad))

saveRDS(encuesta_ipm, file = "Frecuentista_depto/COL/Data/encuesta_ipm.rds")




