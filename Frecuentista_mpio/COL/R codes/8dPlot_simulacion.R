
library(tidyverse)
library(patchwork)

m500 <- openxlsx::read.xlsx( 
  "Frecuentista_depto/COL/Output/Tablas_resultado_teorico/simu_muestreo_MAS_500.xlsx")

m5000 <- openxlsx::read.xlsx( 
  "Frecuentista_depto/COL/Output/Tablas_resultado_teorico/simu_muestreo_MAS_5000.xlsx")

m50000 <- openxlsx::read.xlsx( 
  "Frecuentista_depto/COL/Output/Tablas_resultado_teorico/simu_muestreo_MAS_50000.xlsx")

Complejo <- openxlsx::read.xlsx( 
  "Frecuentista_depto/COL/Output/Tablas_resultado_teorico/simu_muestreo_complejo.xlsx")


resul_simula <- bind_rows(list(
  m500 = m500,
  m5000 = m5000,
  m50000 = m50000,
  Complejo = Complejo
),
.id = "Muestra") %>%
  mutate(Muestra = ordered(Muestra, c("m500","m5000","m50000","Complejo")))

ggplot(resul_simula, aes(x=Muestra, y = sesgo))+ 
  geom_boxplot()+ theme_bw() |

ggplot(resul_simula, aes(x=Muestra, y = smce))+ 
  geom_boxplot() + theme_bw()|

ggplot(resul_simula, aes(x=Muestra, y = cve))+ 
  geom_boxplot() +theme_bw()



