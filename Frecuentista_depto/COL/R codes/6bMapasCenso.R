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

source("Frecuentista_depto/0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
# ipm_censo <- readRDS("Frecuentista_depto/COL/data/censo_completo_ipm.rds")
ipm_censo <- readRDS("Frecuentista_depto/COL/data/censo_ipm2.rds")
ipm_eduacion <- readRDS("Frecuentista_depto/COL/data/ipm_educacion.rds") %>% 
  mutate(ipm_educacion_estimado_MC2 = 1-ipm_educacion_estimado_MC )
ipm_empleo <- readRDS("Frecuentista_depto/COL/data/ipm_empleo.rds")
ipm_estimado_MC <- readRDS("Frecuentista_depto/COL/data/ipm_MC.rds")

ipm_censo %<>% group_by(depto) %>% 
  summarise(ipm_Material = weighted.mean(ipm_Material, n),
            ipm_Saneamiento = weighted.mean(ipm_Saneamiento, n),
            ipm_Energia = weighted.mean(ipm_Energia, n),
            ipm_Internet = weighted.mean(ipm_Internet, n),
            ipm_Agua = weighted.mean(ipm_Agua, n),
            ipm_Hacinamiento = weighted.mean(ipm_Hacinamiento, n)) %>% 
  data.frame() %>% inner_join(ipm_eduacion, by = "depto") %>% 
  inner_join(ipm_empleo, by = "depto") %>% 
  inner_join(ipm_estimado_MC, by = "depto")



## Leer Shape del pais
ShapeSAE <-
  read_sf("Frecuentista_depto/COL/ShapeDeptoCOL/depto.shp") %>%
  rename(depto = DPTO)
###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(ipm_censo,  by = "depto"))

brks_ipm <- c(0,.2, .4,.6,.8, 1)
tmap_options(check.and.fix = TRUE)
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_estimado_MC",
    breaks = brks_ipm,
    title = "IPM-SAE",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Agua",
    breaks = brks_ipm,
    title = "Censo Agua",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_agua.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_educacion_estimado_MC",
    breaks = brks_ipm,
    title = "Educacion-SAE",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_educacion.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_empleo_estimado_MC",
    breaks = brks_ipm,
    title = "Empleo y aseguramiento\n en salud SAE",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/ipm_Empleo.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Energia",
    breaks = brks_ipm,
    title = "IPM (Energía)",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_Energía.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Hacinamiento",
    breaks = brks_ipm,
    title = "IPM (Hacinamiento)",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_Hacinamiento.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Internet",
    breaks = brks_ipm,
    title = "IPM (Internet)",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_Internet.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Material",
    breaks = brks_ipm,
    title = "IPM (Material)",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_material.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Saneamiento",
    breaks = brks_ipm,
    title = "IPM (Saneamiento)",
    palette = "YlOrRd"
  ) + tm_layout(asp = 0)

tmap_save(
  Mapa_ing,
  "Frecuentista_depto/COL/Output/Censo_ipm_saneamiento.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
