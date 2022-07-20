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

source("Frecuentista_mpio/0Funciones/funciones_mrp.R", encoding = "UTF-8")
# Loading data ------------------------------------------------------------
# ipm_censo <- readRDS("Frecuentista_mpio/COL/data/completo_ipm.rds")
ipm_censo <- readRDS("Frecuentista_mpio/COL/data/censo_ipm2.rds")
ipm_eduacion <- readRDS("Frecuentista_mpio/COL/data/ipm_educacion.rds") 
ipm_empleo <- readRDS("Frecuentista_mpio/COL/data/ipm_empleo.rds")
ipm_estimado_MC <- readRDS("Frecuentista_mpio/COL/data/ipm_MC.rds")

ipm_censo %<>% group_by(mpio) %>% 
  summarise(ipm_Material = weighted.mean(ipm_Material, n),
            ipm_Saneamiento = weighted.mean(ipm_Saneamiento, n),
            ipm_Energia = weighted.mean(ipm_Energia, n),
            ipm_Internet = weighted.mean(ipm_Internet, n),
            ipm_Agua = weighted.mean(ipm_Agua, n),
            ipm_Hacinamiento = weighted.mean(ipm_Hacinamiento, n)) %>% 
  data.frame() %>% inner_join(ipm_eduacion, by = "mpio") %>% 
  inner_join(ipm_empleo, by = "mpio") %>% 
  inner_join(ipm_estimado_MC, by = "mpio")



## Leer Shape del pais
ShapeSAE <-
  read_sf("Frecuentista_mpio/COL/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(mpio = COD_DANE,
         nombre = NOM_MUNICI)
###############################################################################
# Colombia
P1_ingresolp <- tm_shape(ShapeSAE %>%
                           left_join(ipm_censo,  by = "mpio"))

brks_ipm <- c(0,.2, .4,.6,.8, 1)
tmap_options(check.and.fix = TRUE)
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_estimado_MC",
    breaks = brks_ipm,
    title = "MPI",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout( 
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Agua",
    breaks = brks_ipm,
    title = "MPI (Water)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_agua.pdf",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_agua.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_agua.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_educacion_estimado_MC",
    breaks = brks_ipm,
    title = "MPI-SAE (Education)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/Sae_ipm_educacion.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/Sae_ipm_educacion.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/Sae_ipm_educacion.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)

###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_empleo_estimado_MC",
    breaks = brks_ipm,
    title = "MPI-SAE (Employment)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Empleo.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Empleo.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Empleo.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Energia",
    breaks = brks_ipm,
    title = "MPI (Energy)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Energía.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Energía.png",
  width = 6920,
  height = 4080,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Energía.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Hacinamiento",
    breaks = brks_ipm,
    title = "MPI (Overcrowding)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Hacinamiento.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )
tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Hacinamiento.png",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Hacinamiento.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Internet",
    breaks = brks_ipm,
    title = "MPI (Internet)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Internet.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Internet.png",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_Internet.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Material",
    breaks = brks_ipm,
    title = "MPI (Construction material)",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_material.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )

tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_material.png",
  width = 6920,
  height = 4080,
  asp = 0
)
tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_material.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
###############################################################################
Mapa_ing <-
  P1_ingresolp + tm_polygons(
    "ipm_Saneamiento",
    breaks = brks_ipm,
    title = "MPI (Sanitation)",
    
  ) + tm_layout(
    legend.only = FALSE,
    legend.height = -0.5,
    legend.width = -0.5,
    asp = 1.5,
    legend.text.size = 3,
    legend.title.size = 3)

# tmap_save(
#   Mapa_ing,
#   "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_saneamiento.pdf",
#   width = 6920,
#   height = 4080,
#   asp = 0
# )


tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_saneamiento.png",
  width = 6920,
  height = 4080,
  asp = 0
)


tmap_save(
  Mapa_ing,
  "Frecuentista_mpio/COL/Output/Plot_Municipio/ipm_saneamiento.jpeg",
  width = 6920,
  height = 4080,
  asp = 0
)
