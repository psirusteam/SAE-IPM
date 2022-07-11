#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Lectura y preparación de las bases de datos           #
# Autor: Stalyn Guerrero  Andrés Gutiérrez              #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(reshape2)
library(stringr)
library(ggalt)
library(gridExtra)
library(scales)
library(formatR)
library(patchwork)
library(survey)
library(srvyr)
theme_set(bayesplot::theme_default())

source(file = "Frecuentista_mpio/0Funciones/funciones_mrp.R",
       encoding = "UTF-8")
# Loading data ------------------------------------------------------------

encuesta_ipm <- readRDS("Frecuentista_mpio/COL/Data/encuesta_ipm.rds")
censo_ipm <- readRDS("Frecuentista_mpio/COL/Data/censo_ipm2.rds")

# Exploratory data analysis -----------------------------------------------

theme_set(theme_bw())

### AGE ###
age_plot <-
  Plot_Compare(dat_censo = censo_ipm,
               dat_encuesta = encuesta_ipm,
               by = "edad")
### Sex ###
sex_plot <-
  Plot_Compare(dat_censo = censo_ipm %>% mutate(sexo = as.character(sexo)),
               dat_encuesta = encuesta_ipm,
               by = "sexo")

### States ###
depto_plot <-
  Plot_Compare(dat_censo = censo_ipm,
               dat_encuesta = encuesta_ipm,
               by = "mpio")

#--- Patchwork in action ---#
(age_plot | sex_plot ) / ( depto_plot)



# Validaciones por dimensión.  --------------------------------------------

## Resultado Nacional 
conteo_censo <- censo_ipm  %>%
  summarise_at(vars(matches("ipm")),~weighted.mean(.,w = n))

diseno <- encuesta_ipm %>%  as_survey_design(weights = fep)

estimacion_encuesta <- diseno  %>% 
  summarise_at(vars(matches("ipm")),survey_mean) %>% 
  dplyr::select(!ends_with("_se"))

temp <- bind_rows(list(cesno = conteo_censo, 
                       encuesta = estimacion_encuesta),.id = "Fuente")
temp <- gather(temp,key = "Dimension",value = "Value",-Fuente)


ggplot(data = temp, aes(x = Dimension,y = Value, fill = Fuente)) +
  geom_bar(stat = "identity", position="dodge")

## Resultados depto
conteo_censo <- censo_ipm %>% group_by(mpio)  %>%
  summarise_at(vars(matches("ipm")),~weighted.mean(.,w = n))


estimacion_encuesta <- diseno %>% group_by(mpio) %>% 
  summarise_at(vars(matches("ipm")),list(dir = survey_mean)) %>% 
  dplyr::select(!ends_with("_se"))

temp <- full_join(conteo_censo, estimacion_encuesta)

par(mfrow = c(2,3))
plot(temp$ipm_Agua, temp$ipm_Agua_dir )  
abline(a=0,b=1,col=2)
plot(temp$ipm_Material, temp$ipm_Material_dir )  
abline(a=0,b=1,col=2)
plot(temp$ipm_Saneamiento, temp$ipm_Saneamiento_dir )  
abline(a=0,b=1,col=2)
plot(temp$ipm_Energia, temp$ipm_Energia_dir )  
abline(a=0,b=1,col=2)
plot(temp$ipm_Internet, temp$ipm_Internet_dir )  
abline(a=0,b=1,col=2)
plot(temp$ipm_Hacinamiento, temp$ipm_Hacinamiento_dir )  
abline(a=0,b=1,col=2)

# Interaction effects  ----------------------------------------------------

theme_set(theme_bw())

### AGE x SEX ###
encuesta_ipm$pobreza <- encuesta_ipm$ingreso
#--- Percentage of people in poverty by AGE x SEX ---#
p_sex_age <-
  plot_interaction(dat_encuesta = encuesta_ipm,
                   by = "sexo",
                   by2 = "edad")

### Level of schooling (LoS) x SEX ###
p_sex_escolar <-
  plot_interaction(dat_encuesta = encuesta_ipm,
                   by = "sexo",
                   by2 = "anoest")

### State x SEX ###
p_sex_depto <-
  plot_interaction(dat_encuesta = encuesta_ipm,
                   by = "sexo",
                   by2 = "depto")

#--- Patchwork in action ---#
(p_sex_age + p_sex_escolar) / p_sex_depto

### Level of schooling (LoS) x AGE ###
p_escolar_edad <-
  plot_interaction(dat_encuesta = encuesta_ipm,
                   by = "anoest",
                   by2 = "edad") +
  theme(legend.position = "bottom") + labs(colour = "anoest")

### State x AGE ###
p_depto_edad <-
  plot_interaction(dat_encuesta = encuesta_ipm,
                   by = "edad",
                   by2 = "depto") +
  theme(legend.position = "bottom") + labs(colour = "Edad")

p_escolar_edad / p_depto_edad

### Level of schooling (LoS) x State ###
p_depto_escolar <-
  plot_interaction(dat_encuesta = encuesta_ipm,
                   by = "anoest",
                   by2 = "depto") +
  theme(legend.position = "bottom") + labs(colour = "anoest")

p_depto_escolar
