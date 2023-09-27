--- 
title: "Metodología bayesiana de estimación desagregada para cualquier IPM"
author: "Andrés Gutiérrez^[Experto Regional en Estadísticas Sociales - Comisión Económica para América Latina y el Caribe (CEPAL) -  andres.gutierrez@cepal.org], Stalyn Guerrero^[Consultor - Comisión Económica para América Latina y el Caribe (CEPAL), guerrerostalyn@gmail.com]"
date: "2023-09-27"
documentclass: book
# bibliography: [CEPAL.bib]
biblio-style: apalike
link-citations: yes
colorlinks: yes
lot: yes
lof: yes
fontsize: 12pt
geometry: margin = 3cm
header-includes: \usepackage[spanish, spanishkw, onelanguage, linesnumbered]{algorithm2e}
github-repo: psirusteam/HHS-Handbook
description: "Metodología bayesiana de estimación del IPM"
knit: "bookdown::render_book"
lang: es
linkcolor: blue
output:
  pdf_document:
    toc: true
    toc_depth: 3
    keep_tex: true
    latex_engine: xelatex
  gitbook:
    df_print: kable
    css: "style.css"
---





# Introducción

El Índice de Pobreza Multidimensional (IPM) es una medida de la pobreza que toma en cuenta múltiples dimensiones de la vida humana, como la salud, la educación, el acceso a los servicios básicos y las condiciones de vida. Fue desarrollado por Sabina Alkire y James Foster en 2007, y se ha utilizado para medir la pobreza en más de 100 países.

El IPM es importante porque ofrece una visión más completa de la pobreza que las medidas tradicionales de la pobreza, que se basan en el ingreso o el consumo. El IPM permite identificar a las personas que son pobres en múltiples dimensiones, y proporciona información sobre las dimensiones de la pobreza que más impactan en la vida de las personas.

El IPM tiene algunas limitaciones. Una limitación es que es difícil de calcular, ya que requiere datos sobre múltiples dimensiones de la pobreza. Otra limitación es que el IPM puede ser subjetivo, ya que depende de las dimensiones que se incluyen en el índice y de los pesos que se asignan a cada dimensión.

En los últimos años, se han desarrollado metodologías más recientes para el cálculo del IPM. Estas metodologías tienen en cuenta algunas de las limitaciones del IPM tradicional, y ofrecen una visión más precisa de la pobreza multidimensional.

Una de las metodologías más recientes es el IPM de Alkire-Foster-Sen (Alkire, Foster y Sen, 2010). Este índice es similar al IPM tradicional, pero tiene en cuenta la desigualdad entre las personas que son pobres. El IPM de Alkire-Foster-Sen también permite identificar a las personas que son pobres en una dimensión, pero no en otras.

El IPM es una herramienta importante para medir la pobreza multidimensional. Ofrece una visión más completa de la pobreza que las medidas tradicionales de la pobreza, y proporciona información sobre las dimensiones de la pobreza que más impactan en la vida de las personas. En los últimos años, se han desarrollado metodologías más recientes para el cálculo del IPM, que ofrecen una visión más precisa de la pobreza multidimensional.

