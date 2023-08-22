#########################################################
# Respuesta correo del 18/08/2022                       #
#########################################################
# 5a. A continuación las estadísticas descriptivas de los tamaños poblacionales en el Censo
aux_temp <- function(x, by = "mpio"){
  data.frame(Agregado = by,
             Min = min(x),
             Q0.25 = quantile(x,prob = 0.25),
             Median = median(x),
             Promedio = mean(x),
             Q0.75 = quantile(x,prob = 0.75),
             Max = max(x))
}
censo <- readRDS("Frecuentista_mpio/COL/Data/censo_ipm2.rds")
Pob <- censo %>% group_by(mpio) %>% summarise(N = sum(n)) %>% 
        mutate(depto = str_sub(mpio, 1,2)) %>% 
        select(depto, mpio, N)

aux_temp(Pob$N)
Pob_depto <- Pob %>% group_by(depto) %>% summarise(N = sum(N)) 
aux_temp(Pob_depto$N, by = "depto")
# 5b. A continuación, las estadísticas descriptivas de los tamaños muestrales en la encuesta.
muestra <- readRDS("Frecuentista_mpio/COL/Data/encuesta_ipm.rds")
n_muestra <- muestra %>% group_by(mpio) %>% summarise(n = n()) %>% 
  mutate(depto = str_sub(mpio, 1,2)) %>% 
  select(depto, mpio, n)

aux_temp(n_muestra$n)
muestra_depto <- n_muestra %>% group_by(depto) %>% summarise(n = sum(n)) 
aux_temp(muestra_depto$n, by = "depto")

list(Descriptivas = 
bind_rows(aux_temp(Pob$N),aux_temp(Pob_depto$N, by = "depto"),
          aux_temp(n_muestra$n, by = "muestra mpio"),
          aux_temp(muestra_depto$n, by = "muestra depto")),
          Población = full_join(Pob, n_muestra)) %>% 
openxlsx::write.xlsx(x = ., 
                     file = "RespuestaCorreo/Output/ConteosPobMuestra.xlsx")  

# 6. Hemos generado las siguientes figuras:
#   6a. Una figura conteniendo seis mapas de COL a nivel municipal censales de 
#   las seis dimensiones conocidas con una sola escala.
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

source("Frecuentista_mpio/0Funciones/funciones_mrp.R",
       encoding = "UTF-8")
# Loading data ------------------------------------------------------------
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
 ShapeSAE %<>% filter(NOM_DEPART == "VALLE DEL CAUCA")
###############################################################################
# Colombia
ipm_censo %<>% gather(key = "Dimension", value = "Value", -mpio)

labels <- c(
"ipm_Material"               = "Poor housing materials",
"ipm_Saneamiento"            = "Lack of sanitation"     ,    
"ipm_Energia"                = "Lack of electricity",
"ipm_Internet"               = "Lack of internet service",
"ipm_Agua"                   = "Lack of drinking water",
"ipm_Hacinamiento"           = "Overcrowding"        ,
"ipm_educacion_estimado_MC"  = "Unfinished education",
"ipm_empleo_estimado_MC"     = "Employment and social protection",
"ipm_estimado_MC"            = "Incidence of multidimensional poverty")

ipm_censo %<>% mutate(Dimension2 = dplyr::recode(Dimension , !!!labels) )

ipm_censo_temp <- ipm_censo 

tmap_options(check.and.fix = TRUE)

brks_ipm <- c(0, .2, .4, .6, .8, 1) 
temp_dinms <- ipm_censo_temp %>% split(.$Dimension2) %>% 
  map(~ShapeSAE %>%
        left_join(.x,  by = "mpio") %>%
        filter(!is.na(Dimension2)) %>% tm_shape())

Mapa_ing<- list()
ii <- "Unfinished education"
for(ii in names(temp_dinms)) {
  Mapa_ing[[ii]] <-  temp_dinms[[ii]] +
    tm_polygons(
      "Value",
      breaks = brks_ipm,
      title = "",
      palette = grey.colors(5,rev = TRUE),
      colorNA = "white",
      
    ) +
    tm_layout(
      legend.show = TRUE,
      # Oculta la leyenda
      main.title = ii,
      main.title.position = c("center", "top"),
      main.title.fontfamily = "Arial",
      main.title.size = 1,  # Tamaño del texto del título
      main.title.fontface = "bold",  # Aplicar negrita al título
      legend.text.size = 1,  # Tamaño del texto de la leyenda
      legend.title.fontfamily = "Arial",  # Cambiar el tipo de letra de la leyenda
      legend.title.fontface = "bold",  # Aplicar negrita a la leyenda
      legend.text.fontface = "bold",
      legend.text.fontfamily = "Arial",
      legend.position = c(-0,0.65)
    ) 
  tmap_save(
    Mapa_ing[[ii]],
    paste0("RespuestaCorreo/Output/",ii,".jpeg"),
    width = 2000,
    height = 1500,
    asp = 0
  )
  
  tmap_save(
    Mapa_ing[[ii]],
    paste0("RespuestaCorreo/Output/",ii,".png"),
    width = 2000,
    height = 1500,
    asp = 0
  )
  
  # tmap_save(
  #   Mapa_ing[[ii]],
  #   paste0("RespuestaCorreo/Output/",ii,".png"),
  #   width = 2500,
  #   height = 1500,
  #   asp = 0
  # )
  
  }

sel_dims <- c(
"Employment and social protection",
"Lack of drinking water",          
"Lack of electricity",             
"Lack of internet service",        
"Lack of sanitation",
"Overcrowding"
)
x11()  
paso <- tmap_arrange(Mapa_ing[sel_dims],nrow = 2, asp = NA) 

tmap_save(
  paso,
  paste0("RespuestaCorreo/Output/Mosaico_","Valle del Cauca",".jpeg"),
  width = 3000,
  height = 2500,
  asp = 0
)

tmap_save(
  paso,
  paste0("RespuestaCorreo/Output/Mosaico_","Valle del Cauca",".png"),
  width = 3000,
  height = 2500,
  asp = 0
)


sel_dims2 <- c("Unfinished education",
     "Employment and social protection", 
     "Incidence of multidimensional poverty")
x11()  
paso <- tmap_arrange(Mapa_ing[sel_dims2],nrow = 2, asp = NA) 

tmap_save(
  paso,
  paste0("RespuestaCorreo/Output/Mosaico_","Valle del Cauca_2",".png"),
  width = 3000,
  height = 2500,
  asp = 0
)

tmap_save(
  paso,
  paste0("RespuestaCorreo/Output/Mosaico_","Valle del Cauca_2",".jpeg"),
  width = 3000,
  height = 2500,
  asp = 0
)


#####################################################################

ShapeSAE <-
  read_sf("Frecuentista_mpio/COL/ShapeDeptoCOL/dv_Municipio.shp") %>%
  rename(mpio = COD_DANE,
         nombre = NOM_MUNICI)

###############################################################################
# Colombia
temp_dinms <- ipm_censo_temp %>% split(.$Dimension2) %>% 
  map(~ShapeSAE %>%
        left_join(.x,  by = "mpio") %>%
        filter(!is.na(Dimension2)) %>% tm_shape())

Mapa_ing<- list()
ii <- "Unfinished education"
for(ii in names(temp_dinms)) {
  Mapa_ing[[ii]] <-  temp_dinms[[ii]] +
    tm_polygons(
      "Value",
      breaks = brks_ipm,
      title = "",
      palette = grey.colors(5,rev = TRUE),
      colorNA = "white",
      
    ) +
    tm_layout(
      legend.show = TRUE,
      # Oculta la leyenda
      main.title = ii,
      main.title.position = c("center", "top"),
      main.title.fontfamily = "Arial",
      main.title.size = 1,  # Tamaño del texto del título
      main.title.fontface = "bold",  # Aplicar negrita al título
      legend.text.size = 1,  # Tamaño del texto de la leyenda
      legend.title.fontfamily = "Arial",  # Cambiar el tipo de letra de la leyenda
      legend.title.fontface = "bold",  # Aplicar negrita a la leyenda
      legend.text.fontface = "bold",
      legend.text.fontfamily = "Arial",
      legend.position = c(-0,0.80)
    ) 
  tmap_save(
    Mapa_ing[[ii]],
    paste0("RespuestaCorreo/Output/",ii,"_COL.jpeg"),
    width = 1500,
    height = 2000,
    asp = 0
  )
  
  tmap_save(
    Mapa_ing[[ii]],
    paste0("RespuestaCorreo/Output/",ii,"_COL.png"),
    width = 1500,
    height = 2000,
    asp = 0
  )
  
  # tmap_save(
  #   Mapa_ing[[ii]],
  #   paste0("RespuestaCorreo/Output/",ii,".png"),
  #   width = 2500,
  #   height = 1500,
  #   asp = 0
  # )
  
}


x11()  
paso <- tmap_arrange(Mapa_ing[sel_dims],nrow = 2, asp = NA) 

tmap_save(
  paso,
  paste0("RespuestaCorreo/Output/Mosaico_","COL",".png"),
  width = 2500,
  height = 3000,
  asp = 0
)

x11()  
paso <- tmap_arrange(Mapa_ing[sel_dims2],nrow = 2, asp = NA) 

tmap_save(
  paso,
  paste0("RespuestaCorreo/Output/Mosaico_","Valle del Cauca_2",".png"),
  width = 3000,
  height = 2500,
  asp = 0
)





# 6b. Una figura conteniendo dos mapas SAE de COL a nivel municipal de las dos dimensiones que se predijeron con una sola escala.
ipm_censo_temp <- ipm_censo %>% 
  filter(Dimension %in% c( "ipm_empleo_estimado_MC",
                           "ipm_educacion_estimado_MC"))

tmap_options(check.and.fix = TRUE)
P1_maps <- tm_shape(ShapeSAE %>%
                      left_join(ipm_censo_temp,  by = "mpio") %>% 
                      filter(!is.na(Dimension2)))
brks_ipm <- c(0,.2, .4,.6,.8, 1)
Mapa_ing <-
  P1_maps + tm_polygons(
    "Value",
    breaks = brks_ipm,
    title = "Value",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout( #legend.outside.position =  "bottom",
    #legend.outside.size = -0.1,
    panel.label.size = 6,
    legend.only = FALSE,
    legend.text.size = 9,
    legend.title.size = 9, 
  ) +
  tm_facets(by = "Dimension2")
Mapa_ing

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6b.jpeg",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6b.pdf",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6b.png",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)


# 6c. Una figura conteniendo un mapa SAE de COL a nivel municipal del IPM final.
ipm_censo_temp <- ipm_censo %>% 
  filter(Dimension %in% c("ipm_estimado_MC"))

tmap_options(check.and.fix = TRUE)
P1_maps <- tm_shape(ShapeSAE %>%
                      left_join(ipm_censo_temp,  by = "mpio") %>% 
                      filter(!is.na(Dimension2)))
brks_ipm <- c(0,.2, .4,.6,.8, 1)
Mapa_ing <-
  P1_maps + tm_polygons(
    "Value",
    breaks = brks_ipm,
    title = "Value",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout( #legend.outside.position =  "bottom",
    #legend.outside.size = -0.1,
    panel.label.size = 6,
    legend.only = FALSE,
    legend.text.size = 9,
    legend.title.size = 9, 
  ) +
  tm_facets(by = "Dimension2")
Mapa_ing

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6c.jpeg",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6c.pdf",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6c.png",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

# 6d. Una figura conteniendo un mapa de los CV SAE de COL a nivel municipal del IPM final.
censo_CV <- openxlsx::read.xlsx(xlsxFile = "Frecuentista_mpio/COL/Output/Comparando_dir_censo_sae/Estimacion_mpio.xlsx")
censo_CV_temp <- censo_CV %>% select(mpio, IPM_cv) %>% rename( "MPI (CV)" = IPM_cv)

P1_maps <- tm_shape(ShapeSAE %>%
                      left_join(censo_CV_temp,  by = "mpio") %>% 
                      filter(!is.na(`MPI (CV)`)))

brks_ipm <- c(0, 5,10,15,20) 
Mapa_ing <-
  P1_maps + tm_polygons(
    "MPI (CV)",
    breaks = brks_ipm,
    title = "CV",
    palette = grey.colors(4,rev = TRUE),
    colorNA = "white",
    
  ) +
  tm_layout(
    legend.show = TRUE,
    # Oculta la leyenda
    main.title = "",
    main.title.position = c("center", "top"),
    main.title.fontfamily = "Arial",
    main.title.size = 1,  # Tamaño del texto del título
    main.title.fontface = "bold",  # Aplicar negrita al título
    legend.text.size = 1,  # Tamaño del texto de la leyenda
    legend.title.fontfamily = "Arial",  # Cambiar el tipo de letra de la leyenda
    legend.title.fontface = "bold",  # Aplicar negrita a la leyenda
    legend.text.fontface = "bold",
    legend.text.fontfamily = "Arial",
    legend.position = c(-0,0.85)
  ) 
Mapa_ing

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/Figure5.jpeg",
  width = 3000,
  height = 2500,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6d.pdf",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6d.png",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)


# 6e. Una figura conteniendo ocho mapas del Valle del Cauca a nivel municipal de las ocho dimensiones con una sola escala.
ShapeSAE_TEPM <- ShapeSAE %>% filter(NOM_DEPART == "VALLE DEL CAUCA")
ipm_censo_temp <- ipm_censo %>% 
  filter(!Dimension %in% c("ipm_estimado_MC"))

P1_maps <- tm_shape(ShapeSAE_TEPM %>%
                      left_join(ipm_censo_temp,  by = "mpio") %>% 
                      filter(!is.na(Dimension2)))
brks_ipm <- c(0,.2, .4,.6,.8, 1)
Mapa_ing <-
  P1_maps + tm_polygons(
    "Value",
    breaks = brks_ipm,
    title = "Value",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout( #legend.outside.position =  "bottom",
    #legend.outside.size = -0.1,
    panel.label.size = 6,
    legend.only = FALSE,
    legend.text.size = 9,
    legend.title.size = 9, 
  ) +
  tm_facets(by = "Dimension2")
Mapa_ing

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6e.jpeg",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6e.pdf",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6e.png",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

# 6f. Una figura conteniendo un mapa del Valle del Cauca a nivel municipal del IPM final.

ipm_censo_temp <- ipm_censo %>% 
  filter(Dimension %in% c("ipm_estimado_MC"))

P1_maps <- tm_shape(ShapeSAE_TEPM %>%
                      left_join(ipm_censo_temp,  by = "mpio") %>% 
                      filter(!is.na(Dimension2)))
brks_ipm <- c(0,.2, .4,.6,.8, 1)
Mapa_ing <-
  P1_maps + tm_polygons(
    "Value",
    breaks = brks_ipm,
    title = "Value",
    palette = "YlOrRd",
    colorNA = "white"
  ) + tm_layout( #legend.outside.position =  "bottom",
    #legend.outside.size = -0.1,
    panel.label.size = 6,
    legend.only = FALSE,
    legend.text.size = 9,
    legend.title.size = 9, 
  ) +
  tm_facets(by = "Dimension2")
Mapa_ing

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6f.jpeg",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6f.pdf",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)

tmap_save(
  Mapa_ing,
  "RespuestaCorreo/Output/6f.png",
  width = 6920*3,
  height = 4080*3,
  asp = 0
)
