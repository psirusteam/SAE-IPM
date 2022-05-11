#########################################################
# Proyecto MRP - Leave no one behind                    #
# Mapas de pobreza CEPAL                                #
# Modelo Multinivel Bayesiano                           #
# Autor: Stalyn Guerrero  &  Andrés Gutiérrez           #
#########################################################

rm(list = ls())

# Loading required libraries ----------------------------------------------

library(tidyverse)
library(magrittr)
library(patchwork)
library(sp)
library(sf)
library(tmap)
library(RColorBrewer)
library(maptools)
library(tmaptools)

# Loading data ------------------------------------------------------------
# ipm_censo <- readRDS("Frecuentista_depto/COL/data/censo_completo_ipm.rds")
estimacion_dir  <- openxlsx::read.xlsx("Frecuentista_depto/COL/Output/Educacion_y_Empleo_dir.xlsx")
## Leer Shape del pais
ShapeSAE <-
  read_sf("Frecuentista_depto/COL/ShapeDeptoCOL/depto.shp") %>%
  rename(depto = DPTO)
###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(estimacion_dir,  by = "depto"))

brks_ipm <- c(0,.2, .4,.6,.8, 1)
tmap_options(check.and.fix = TRUE)
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Educacion",
    breaks = brks_ipm,
    title = "Educacion directa",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Educacion_dir.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Educacion_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Educacion directa (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Educacion_dir_cve.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)


###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Empleo",
    breaks = brks_ipm,
    title = "Empleo y aseguramiento\n en salud directa",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Empleo_dir.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Empleo_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Empleo y aseguramiento\n en salud directa (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Empleo_dir_cve.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

################################################################################
#### mapeando las estimaciones SAE 
################################################################################

smce_educacion  <- openxlsx::read.xlsx("Frecuentista_depto/COL/Output/educacion_smce_MC.xlsx") %>% 
                  mutate(educacion_cv = smce/ipm_educacion_estimado_MC ) %>%
  rename(smce_educacion = smce) 

smce_empleo  <- openxlsx::read.xlsx("Frecuentista_depto/COL/Output/empleo_smce_MC.xlsx") %>% 
  mutate(empleo_cv = smce/ipm_empleo_estimado_MC ) %>% 
  rename(smce_empleo = smce)

smce_ipm  <- openxlsx::read.xlsx("Frecuentista_depto/COL/Output/smce_MC.xlsx") %>% 
  mutate(ipm_cv = smce/ipm_estimado_MC ) %>% 
  rename(smce_ipm = smce)

estimado_cv_sae <- inner_join(smce_educacion,smce_empleo, by = "depto") %>% 
  inner_join(smce_ipm, by = "depto") %>% 
  select(depto, matches("_cv"))

###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(estimado_cv_sae,  by = "depto"))

Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "educacion_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Educacion SAE (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Educacion_SAE_cv.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)


###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "empleo_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Empleo y aseguramiento\n en salud SAE (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Empleo_SAE_cve.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "IPM-SAE (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/IPM_SAE_cve.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
