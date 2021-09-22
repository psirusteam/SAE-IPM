
###--- Cleaning R environment ---###

rm(list = ls())
gc()

#################
### Librerías ###
#################

library(tidyverse)
library(dplyr)
library(readstata13)
library(sas7bdat)

################################################################################
                   ### Lectura de bases de datos: GEIH ###
################################################################################

COL18_2 <- readRDS("Input/COL18_baseIPM.rds")

################################################################################
###---------------------- Creando variables dicotómicas ---------------------###
################################################################################

IPM <- COL18_2 %>% transmute(
  
  ###------------------ Identificador de hogar y personas -------------------### 
  
  id_hogar, id_pers,
  
  ###--------------------------- Área geográfica ----------------------------### 
  
  area = areageo2,
  
  ###------------------------- Factores de expansión ------------------------### 
  
  factorex, edad, sexo,
  
  ###---- NBI: p5090 La vivienda ocupada por este hogar es: a. Propia,   ----###
  ###---  totalmente pagada b. Propia, la están pagando c.En arriendo o, ----###
  
  nbi_ten = case_when(p5090 %in% c(5,6) ~ 1,
                      p5090 == 9 ~ NA_real_,
                      TRUE ~ 0),
  
  ###- p4030s5: ¿Con cuáles de los siguientes servicios cuenta la vivienda? -###  
  ###-     NBI: Acueducto 1 Sí 2 No                                         -###                             
  
  nbi_agua = case_when(p4030s5 == 2 & area == 2 ~ 1,
                       p4030s5 == 1 ~ 1,
                       p5050 %in% c(1, 2, 3) ~ 0,
                       p5050 %in% c(4, 5, 6) ~ 0,
                       p5050 %in% c(7, 8, 9) & area == 1 ~ 1,
                       p5050 %in% c(7, 8, 9) & area == 2 ~ 0,
                       p5050 == 10 ~ 0,
                       is.na(p5050) ~ NA_real_,
                       p4030s5 == 2 & area == 1 ~ 1),
  
  ###--- P5020: El servicio sanitario que utiliza el hogar es: a. Inodoro ---###
  ###---        conectado a alcantarillado b. Inodoro conectado a pozo,   ---### 
  
  nbi_saneamiento = case_when(p5020 %in% c(1,2) ~ 0,
                              p5020 %in% c(2,3,4,5) & area == 1 ~ 1,
                              p5020 %in% c(5) & area == 2 ~ 1,
                              p5020 == 6 ~ 1,
                              p5030 == 2 ~ 1),
  
  ###- p4030s1: ¿Con cuáles de los siguientes servicios cuenta la vivienda? -###  
  ###-     NBI: Energía eléctrica 1 Sí 2 No                                 -### 
  
  nbi_elect =  case_when(p4030s1 %in% c(2) ~ 0,
                         p4030s1 == 1 ~ 1,
                         p4030s1 == 0 ~ NA_real_),
  
  ###- p4020: ¿cuál es el material predominante de los pisos de la vivienda?-###
  
  nbi_piso =   case_when(p4020 %in% c(2:7) ~ 0,
                         p4020 == 1 ~ 1,
                         TRUE ~ NA_real_),
  
  ###- p4010: ¿cuál es el material predominante de las paredes exteriores de-###
  ###-        la vivienda?                                                  -###
  
  nbi_pared =  case_when(p4010 %in% c(6:9) ~ 1,
                         TRUE ~ 0),
  
  ###- p5080: ¿con qué energía o combustible cocinan principalmente en este -###
  ###-        hogar? a. Electricidad b. Petróleo, gasolina, kerosene. etc.  -###
  
  nbi_combus =  case_when(p5080 %in% c(2,5,6,7) ~ 1,
                        TRUE ~ 0),
  
  ###---------------- p6160: ¿sabe leer y escribir? 1 Sí 2 No ---------------###
  
  lee_ee = case_when(p6160 == 1 ~ 1,
                     p6160 == 2 ~ 0,
                     is.na(p6160) ~ NA_real_),
  
  ###-- p6170: ¿actualmente ... Asiste a la escuela, colegio o universidad? -###
  ###--        1 Sí 2 No                                                    -###
  
  asiste_ee = case_when(p6170 == 1 ~ 1,
                       p6170 == 2 ~ 0,
                       is.na(p6170) ~ NA_real_),
  
  condactr = case_when(condact == -1 ~ NA_real_,
                       condact == 1 ~ 1,
                       condact == 2 ~ 2,
                       condact == 3 ~ 2,
                       condact == 4 ~ 3,
                       condact == 5 ~ 4,
                       condact == 6 ~ 7,
                       condact == 7 ~ 6),
  
  ###--------- Viviendas con materiales inadecuados: Paredes o pisos --------###
  
  nbi_matviv = case_when((nbi_piso == 1 | nbi_pared ==1) ~ 1,
                         (nbi_piso == 0 & nbi_pared ==0) ~ 0,
                         p4000 %in% c(5,6) ~ 1,
                         p4010 == 5 ~ 1,
                         p4020 == 3 ~ 1),
  
  ###-------------------- Viviendas con energía eléctrica -------------------###
  
  nbi_energia = case_when(nbi_elect == 1 | nbi_combus == 1 ~ 1,
                          nbi_elect == 0 & nbi_combus == 0 ~ 0,
                          is.na(nbi_elect) & is.na(nbi_combus) ~ NA_real_),
  
  ###------------------- Asistencia a institución educativa -----------------###
  
  nbi_asistencia_ee = ifelse(asiste_ee == 0 & (edad >= 4 & edad <= 18), 1, 0),
  
  ###-- p5000: Incluyendo sala-comedor ¿de cuántos cuartos en total dispone -###
  ###--        este hogar?                                                  -### 
  
  ncuartos_ee = ifelse(p5000 == -1, NA_real_, p5000),
  
  ###--- p5010: ¿en cuántos de esos cuartos duermen las personas de este  ---###
  ###---         hogar?                                                   ---###
  
  ndormitorios_ee = ifelse(p5010 == -1, NA_real_, p5010),
  
  ###------------------- Personas viviendo en hacinamiento ------------------###
  
  nbi_hacina21 = case_when(ndormitorios_ee == 0 ~ 1,
                           (persindo/ndormitorios_ee) > 2 ~ 1,
                           TRUE ~ 0),
  
  ###-------------------- Personas con acceso a computador ------------------###
  
  nbi_compuhog_ee = case_when(p5210s16 == 2 ~ 1,
                              p5210s16 == 1 ~ NA_real_),
  
  ###---- ¿Cuáles de los siguientes servicios o bienes en uso, posee este ---### 
  ###----  hogar? c. Servicio de Internet 1 Sí 2 No                       ---###
  
  nbi_interhog_ee = ifelse(p5210s3 == 2, 1, 0),
  
  ###---------------------- Número de años de educación ---------------------###
  
  niveduc_ee = case_when(anoest < 5 ~ 1,
                         anoest == 5 ~ 2,
                         anoest > 5 & anoest < 9 ~ 3,
                         anoest >= 9 & anoest < 11 ~ 4,
                         anoest == 11 ~ 5,
                         anoest > 11 & anoest < 16 ~ 6,
                         anoest >= 16 ~ 7),
  
  ###---------------------------- Rezago escolar ----------------------------###
  
  rezagado = case_when(edad < 6 | edad > 16 ~ NA_real_,
                       asiste_ee == 1 & (edad - 6 - anoest) > 2 ~ 1),
  rezagado = ifelse(is.na(rezagado), 0, 1),
  
  ###-------------------- Nivel de escolaridad según edad -------------------###
  
  nbi_logro_sc = case_when(niveduc_ee >= 5 & edad >=  19 & edad <= 29 ~ 0,
                           niveduc_ee < 5 & edad >=  19 & edad <= 29 ~ 1,
                          TRUE  ~ NA_real_),
  
  nbi_logro_bs = case_when(niveduc_ee > 3 & edad >=  30 & edad <= 59 ~ 0,
                           niveduc_ee <= 3 & edad >=  30 & edad <= 59 ~ 1,
                           TRUE  ~ NA_real_),
  
  ###------ p6920: ¿está cotizando actualmente a un fondo de pensiones? -----###
  ###------ p6980s1: Aportar a un fondo de pensiones obligatorias       -----###
  ###------ p6980s2: Aportar a un fondo de pensiones voluntarias        -----###
  ###------ p7420s1: Aportar a un fondo de pensiones obligatorias       -----###
  ###------ p7420s2: Aportar a un fondo de pensiones voluntarias        -----###
  ###------ p7460: ¿está afiliado actualmente a un fondo de pensiones?  -----###
  
  afilia = ifelse(p6920 == 1 | p6980s1 == 1 | p6980s2 == 1 | p7420s1 == 1 | 
                  p7420s2 == 1 | p7460 == 1, 1, 0),
  
  cotiza = ifelse(p6920 == 1 | p6980s1 == 1 | p6980s2 == 1 | p7420s1 == 1 | 
                  p7420s2 == 1, 1, 0),
  
  Tptotal = case_when(is.na(iof2) & is.na(iof2es) ~ 0,
                      iof2 > 0 | iof2es > 0 ~ 1,
                      TRUE ~ 0),
  
  ###----------------- Variable indicadora: personas adultas ----------------###
  
  adultos = ifelse((edad >= 18 & edad < 60 & sexo == 2) | 
                     (edad >= 18 & edad < 65 & sexo == 1), 1, 0),
  
  ###-------------- Variable indicadora: personas adulto mayor --------------###
  
  a_mayores = ifelse((edad >= 60 & sexo == 2)|(edad >= 65 & sexo == 1), 1, 0),
  
  ###--------------- Acceso a las TIC's: Computador e internet --------------###
  
  nbi_tics_h = ifelse(nbi_compuhog_ee == 1 & nbi_interhog_ee == 1, 1, 0),
  
  ###----- Acceso adecuado a servicio de saneamiento: Agua y saneamiento ----###
  
  nbi_asan = ifelse((nbi_agua == 1 | nbi_saneamiento == 1), 1, 0),
  
  ###------ Inasistencia a institución educativa y bajo nivel educativo -----###
  
  noasis18 = ifelse(nbi_asistencia_ee == 0 & niveduc_ee < 5 & edad >= 18 & 
                      edad <= 19, 1, 0),
  
  ###---------------------------- Rezago escolar ----------------------------###
  
  nbi_asrez = ifelse((nbi_asistencia_ee == 1 | rezagado == 1 | noasis18 == 1) & 
                      edad < 20, 1, 0),
  
  ###---------------------------- Logro educativo ---------------------------###
  
  nbi_conclued = ifelse((nbi_logro_sc == 1 | nbi_logro_bs == 1 | noasis18 == 1), 
                        1, 0),
  
  ###----------------------- Analfabetismo en adultos -----------------------###
  
  analfab_ee = case_when(lee_ee == 0 ~ 1,
                         lee_ee == 1 ~ 0,
                         is.na(lee_ee) ~ NA_real_),
  
  amay_analfab = ifelse((analfab_ee == 1 & a_mayores == 1), 1, 0),
  
  ###--------------------------- Personas ocupadas --------------------------###
  
  ocup_priv2 =ifelse((condactr == 2 | categ5_p == 5 | (cotiza == 0 & condactr == 1) | 
                        condactr == 4) , 1, 0),
  
  ###------------------------- Dependencia económica ------------------------###
  
  nbi_pen = case_when(a_mayores == 0 ~ NA_real_,
                      a_mayores == 1 & Tptotal == 0 ~ 1,
                      TRUE ~ 0),
  
  ##############################################################################
  ###-------------------------- Dimensión VIVIENDA --------------------------###
  ##############################################################################
  
  g0_A_nbi_matviv = ifelse(adultos == 1,(1/8) * nbi_matviv, 0),
  
  g0_A_nbi_hacina21 = ifelse(adultos == 1,(1/8) * nbi_hacina21, 0),
  
  g0_AM_nbi_matviv = ifelse(a_mayores == 1,(1/8) * nbi_matviv, 0),
  
  g0_AM_nbi_hacina21 = ifelse(a_mayores == 1,(1/8) * nbi_hacina21, 0),
  
  # Gaps de dimension servicios
  
  g0_A_nbi_tics_h = ifelse(adultos == 1,(1/16)*nbi_tics_h,0),
  g0_A_nbi_agua = ifelse(adultos == 1,(1/16)*nbi_agua,0),
  g0_A_nbi_nbi_saneamiento = ifelse(adultos == 1,(1/16)*nbi_saneamiento,0),
  g0_A_nbi_nbi_energia = ifelse(adultos == 1,(1/16)*nbi_energia,0), 
  
  g0_AM_nbi_tics_h = ifelse(a_mayores == 1,(1/16)*nbi_tics_h,0),
  g0_AM_nbi_agua = ifelse(a_mayores == 1,(1/16)*nbi_agua,0),
  g0_AM_nbi_nbi_saneamiento = ifelse(a_mayores == 1,(1/16)*nbi_saneamiento,0),
  g0_AM_nbi_nbi_energia = ifelse(a_mayores == 1,(1/16)*nbi_energia,0), 
  
  # Gaps IPM adultos
  
  g0_A_nbi_conclued = ifelse(adultos == 1,(1/4)*nbi_conclued ,0),
  g0_A_ocup_priv2 = ifelse(adultos == 1,(1/4)*ocup_priv2,0),
  
  # Gaps Adultos mayores
  
  g0_AM_amay_analfab  = ifelse(adultos == 1,(1/4)*amay_analfab ,0),
  g0_AM_nbi_pen = ifelse(adultos == 1,(1/4)*nbi_pen,0),
  )

# Cantidad privaciones adulto
IPM$C_A = rowSums(IPM %>% select(starts_with("g0_A")), na.rm = T)
IPM$C_A = ifelse(IPM$adultos == 0, NA, IPM$C_A)
# Cantidad privaciones adulto mayor
IPM$C_AM = rowSums(IPM %>% select(starts_with("g0_AM")), na.rm = T)
IPM$C_AM = ifelse(IPM$a_mayores == 0, NA, IPM$C_AM)
summary(IPM$C_A)
summary(IPM$C_AM)

mean(IPM$C_A)
weighted.mean(IPM$C_A, IPM$factorex)


mean(IPM$C_AM)
weighted.mean(IPM$C_AM, IPM$factorex)

## 
IPM$AM_PMD = NA
IPM$A_PMD = NA

for(k in 1:100){
  print(k)
  #IPM$AM_PMD = ifelse(IPM$C_AM >= k/100, paste0("Adulto mayor PM con k =",k),  IPM$AM_PMD)
  #IPM$A_PMD = ifelse(IPM$C_A >= k/100, paste0("Adulto PM con k =",k),  IPM$A_PMD)
  
  IPM$AM_PMD = ifelse(IPM$C_AM >= k/100, k,  IPM$AM_PMD)
  IPM$A_PMD = ifelse(IPM$C_A >= k/100, k,  IPM$A_PMD)
  
}
IPM$AM_PMD = ifelse(IPM$adultos == 1, NA, IPM$AM_PMD )


IPM$c_AM_PMD = ifelse(is.na(IPM$AM_PMD), 0, IPM$AM_PMD)
IPM$c_A_PMD = ifelse(is.na(IPM$A_PMD), 0, IPM$A_PMD)


IPM %>% filter(adultos == 1) %>% mutate(sexo = ifelse(sexo == 1, "Hombre", "Mujer")) %>%
  group_by(sexo) %>%
  summarise(IPM_A = weighted.mean(c_A_PMD, factorex))


IPM %>% filter(a_mayores == 1) %>% mutate(sexo = ifelse(sexo == 1, "Hombre", "Mujer")) %>%
  group_by(sexo) %>%
  summarise(IPM_AM = weighted.mean(c_AM_PMD, factorex))

saveRDS(IPM, "Input/IPM.rds")

