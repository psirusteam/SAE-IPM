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
estimacion_dir  <- openxlsx::read.xlsx("Frecuentista_depto/COL/Output/Departamento/Educacion_y_Empleo_dir.xlsx")
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
    title = "Education direct",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_dir.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_dir.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)


Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Educacion_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Education direct (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_dir_cve.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_dir_cve.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)


###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Empleo",
    breaks = brks_ipm,
    title = "Direct estimate for employment\nand health insurance",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_dir.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_dir.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "Empleo_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Direct estimate (cv) for employment\nand health insurance",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_dir_cve.png",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_dir_cve.jpeg",
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
    title = "Estimation of the cv \nof SAE for education",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_SAE_cv.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_SAE_cv.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "empleo_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "Estimation of the cv of SAE\nemployment and health insurance",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_SAE_cve.png",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_SAE_cve.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_cv",
    breaks = c(0.00, 0.05, 0.2, 1),
    title = "MPI-SAE (cv)",
    palette = "BuGn",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/IPM_SAE_cve.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/IPM_SAE_cve.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

##################################################################
estimado_MC_sae <- inner_join(smce_educacion,smce_empleo, by = "depto") %>% 
  inner_join(smce_ipm, by = "depto") %>% 
  select(depto, matches("_MC"))

###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(estimado_MC_sae,  by = "depto"))

Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_educacion_estimado_MC",
    breaks = c(0,.2, .4,.6,.8, 1),
    title = "Estimation of SAE for education",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_SAE.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Educacion_SAE.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_empleo_estimado_MC",
    breaks = c(0,.2, .4,.6,.8, 1),
    title = "Estimation of SAE\nemployment and health insurance",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_SAE.png",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/Empleo_SAE.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_estimado_MC",
    breaks = c(0,.2, .4,.6,.8, 1),
    title = "MPI-SAE",
    palette = "YlOrRd",
    colorNA = "white",
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.3,
    legend.width = -0.3,
    legend.position = c(0.1, 0.1),
    asp = 0,
    legend.title.size = 4,
    legend.text.size =  4
  )

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/IPM_SAE.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Departamento/IPM_SAE.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

