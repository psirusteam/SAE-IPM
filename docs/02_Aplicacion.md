

# Aplicación: Índice de Pobreza Multidimensional en Colombia. 

Nos centramos en la incidencia de la pobreza multidimensional descrito previamente. En este caso, requerimos $K = 8$ indicadores que se miden como privaciones: $y_{di}^{k} = 1$ si la persona tiene la privación y $y_{di}^{k} = 0$ si la persona no ha tenido la privación.

El índice requiere información para cada individuo $i = 1, \ldots, N_d$ en los dominios $d = 1, \ldots, D$, donde $N_d$ denota el tamaño de la población del dominio $d$.

La función indicadora $I(\cdot)$ es igual a 1 cuando se cumple la condición $q_{di} > z$. Para este estudio, utilizamos el valor de 0.4 para $z$, es decir, $I(\cdot)$ es igual a 1 cuando $q_{di} \ge 0.4$. $q_{di}$ es una cantidad ponderada que considera los $K = 8$ indicadores que conforman el índice. El valor de $q_{di}$ el dominio $d$ se calcula como:
  
$$
  q_{di} = 0.1\sum_{k=1}^{6}y_{di}^{k} +  0.2(y_{di}^{7} + y_{di}^{8})
$$
  
  Donde: 
  a. $y_{di}^{1}$ = Privación en material de construcción de la vivienda

  b. $y_{di}^{2}$ = Hacinamiento en el hogar. 

  c. $y_{di}^{3}$ = Privación de acceso al agua potable. 

  d. $y_{di}^{4}$ = Privación en saneamiento.

  e. $y_{di}^{5}$ = Acceso al servicio energía eléctrica. 

  f. $y_{di}^{6}$ = Acceso al servicio de internet.

  g. $y_{di}^{7}$ = Privación de la educación. 

  h. $y_{di}^{8}$ = Privación del empleo y la protección social.  

Note que, la primera parte de la suma considera los indicadores de las dimensiones de vivienda, agua y saneamiento, energía y conectividad. La segunda parte, los indicadores de las dimensiones de educación y empleo y protección social. 

Los datos de la encuesta y el censo han sido preparados previamente, la información sobre la cual realizaremos la predicción corresponde a Colombia en el 2019 


```r
encuesta_ipm <-
  readRDS("Modelo_bayes_HxA/COL/Data/encuesta_COL.rds")
statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds") %>% 
  rename(depto = dam, mpio = dam2)

byAgrega <- c("depto",
              "mpio",
              "area",
              "sexo",
              "etnia",
              "anoest",
              "edad",
              "condact3")
```

Agregando la información para los municipios de Colombia para los indicadores que conformarán el IPM


```r
names_ipm <- grep(pattern = "ipm", names(encuesta_ipm),value = TRUE)

encuesta_df <- map(setNames(names_ipm,names_ipm),
    function(y){
  encuesta_ipm$temp <- encuesta_ipm[[y]]
  encuesta_ipm %>% 
  group_by_at(all_of(byAgrega)) %>%
  summarise(n = n(),
            yno = sum(temp),
            ysi = n - yno, .groups = "drop") %>% 
    inner_join(statelevel_predictors_df,
                              by = c("depto","mpio"))
})
```

La base resultante quedan de la siguiente forma:

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-3)Privaci'on en material de construcci'on de la vivienda</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> depto </th>
   <th style="text-align:left;"> mpio </th>
   <th style="text-align:left;"> area </th>
   <th style="text-align:left;"> sexo </th>
   <th style="text-align:left;"> etnia </th>
   <th style="text-align:left;"> anoest </th>
   <th style="text-align:left;"> edad </th>
   <th style="text-align:right;"> condact3 </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> yno </th>
   <th style="text-align:right;"> ysi </th>
   <th style="text-align:right;"> area1 </th>
   <th style="text-align:right;"> sexo2 </th>
   <th style="text-align:right;"> edad2 </th>
   <th style="text-align:right;"> edad3 </th>
   <th style="text-align:right;"> edad4 </th>
   <th style="text-align:right;"> edad5 </th>
   <th style="text-align:right;"> etnia2 </th>
   <th style="text-align:right;"> anoest2 </th>
   <th style="text-align:right;"> anoest3 </th>
   <th style="text-align:right;"> anoest4 </th>
   <th style="text-align:right;"> etnia1 </th>
   <th style="text-align:right;"> tiene_acueducto </th>
   <th style="text-align:right;"> piso_tierra </th>
   <th style="text-align:right;"> alfabeta </th>
   <th style="text-align:right;"> hacinamiento </th>
   <th style="text-align:right;"> tasa_desocupacion </th>
   <th style="text-align:right;"> luces_nocturnas </th>
   <th style="text-align:right;"> cubrimiento_cultivo </th>
   <th style="text-align:right;"> cubrimiento_urbano </th>
   <th style="text-align:right;"> modificacion_humana </th>
   <th style="text-align:right;"> accesibilidad_hospitales </th>
   <th style="text-align:right;"> accesibilidad_hosp_caminado </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1434 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1429 </td>
   <td style="text-align:right;"> 0.9979 </td>
   <td style="text-align:right;"> 0.5219 </td>
   <td style="text-align:right;"> 0.2690 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.3810 </td>
   <td style="text-align:right;"> 0.2938 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 22.0069 </td>
   <td style="text-align:right;"> 9.1869 </td>
   <td style="text-align:right;"> 19.7751 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 61.3823 </td>
   <td style="text-align:right;"> 259.2423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 47001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1432 </td>
   <td style="text-align:right;"> 17 </td>
   <td style="text-align:right;"> 1415 </td>
   <td style="text-align:right;"> 0.9150 </td>
   <td style="text-align:right;"> 0.5158 </td>
   <td style="text-align:right;"> 0.2705 </td>
   <td style="text-align:right;"> 0.2125 </td>
   <td style="text-align:right;"> 0.1913 </td>
   <td style="text-align:right;"> 0.0719 </td>
   <td style="text-align:right;"> 0.0354 </td>
   <td style="text-align:right;"> 0.2539 </td>
   <td style="text-align:right;"> 0.4149 </td>
   <td style="text-align:right;"> 0.1837 </td>
   <td style="text-align:right;"> 0.0169 </td>
   <td style="text-align:right;"> 0.3002 </td>
   <td style="text-align:right;"> 0.0312 </td>
   <td style="text-align:right;"> 0.0364 </td>
   <td style="text-align:right;"> 0.2845 </td>
   <td style="text-align:right;"> 0.0015 </td>
   <td style="text-align:right;"> 4.3364 </td>
   <td style="text-align:right;"> 0.4577 </td>
   <td style="text-align:right;"> 1.7512 </td>
   <td style="text-align:right;"> 0.3278 </td>
   <td style="text-align:right;"> 210.3355 </td>
   <td style="text-align:right;"> 611.8750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1387 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1385 </td>
   <td style="text-align:right;"> 0.9979 </td>
   <td style="text-align:right;"> 0.5219 </td>
   <td style="text-align:right;"> 0.2690 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.3810 </td>
   <td style="text-align:right;"> 0.2938 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 22.0069 </td>
   <td style="text-align:right;"> 9.1869 </td>
   <td style="text-align:right;"> 19.7751 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 61.3823 </td>
   <td style="text-align:right;"> 259.2423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1326 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 1319 </td>
   <td style="text-align:right;"> 0.9979 </td>
   <td style="text-align:right;"> 0.5219 </td>
   <td style="text-align:right;"> 0.2690 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.3810 </td>
   <td style="text-align:right;"> 0.2938 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 22.0069 </td>
   <td style="text-align:right;"> 9.1869 </td>
   <td style="text-align:right;"> 19.7751 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 61.3823 </td>
   <td style="text-align:right;"> 259.2423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1280 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 1273 </td>
   <td style="text-align:right;"> 0.9993 </td>
   <td style="text-align:right;"> 0.5208 </td>
   <td style="text-align:right;"> 0.2558 </td>
   <td style="text-align:right;"> 0.2131 </td>
   <td style="text-align:right;"> 0.2146 </td>
   <td style="text-align:right;"> 0.0968 </td>
   <td style="text-align:right;"> 0.0521 </td>
   <td style="text-align:right;"> 0.2324 </td>
   <td style="text-align:right;"> 0.3976 </td>
   <td style="text-align:right;"> 0.2411 </td>
   <td style="text-align:right;"> 0.0012 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 0.0232 </td>
   <td style="text-align:right;"> 0.2084 </td>
   <td style="text-align:right;"> 0.0029 </td>
   <td style="text-align:right;"> 58.0251 </td>
   <td style="text-align:right;"> 3.8798 </td>
   <td style="text-align:right;"> 50.9581 </td>
   <td style="text-align:right;"> 0.8114 </td>
   <td style="text-align:right;"> 3.2552 </td>
   <td style="text-align:right;"> 21.7908 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 23001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1236 </td>
   <td style="text-align:right;"> 83 </td>
   <td style="text-align:right;"> 1153 </td>
   <td style="text-align:right;"> 0.8192 </td>
   <td style="text-align:right;"> 0.5149 </td>
   <td style="text-align:right;"> 0.2631 </td>
   <td style="text-align:right;"> 0.2146 </td>
   <td style="text-align:right;"> 0.1961 </td>
   <td style="text-align:right;"> 0.0761 </td>
   <td style="text-align:right;"> 0.0170 </td>
   <td style="text-align:right;"> 0.2857 </td>
   <td style="text-align:right;"> 0.3634 </td>
   <td style="text-align:right;"> 0.1920 </td>
   <td style="text-align:right;"> 0.0072 </td>
   <td style="text-align:right;"> 0.1251 </td>
   <td style="text-align:right;"> 0.1412 </td>
   <td style="text-align:right;"> 0.0657 </td>
   <td style="text-align:right;"> 0.2275 </td>
   <td style="text-align:right;"> 0.0010 </td>
   <td style="text-align:right;"> 3.8284 </td>
   <td style="text-align:right;"> 19.0431 </td>
   <td style="text-align:right;"> 1.0209 </td>
   <td style="text-align:right;"> 0.4172 </td>
   <td style="text-align:right;"> 48.4391 </td>
   <td style="text-align:right;"> 234.8668 </td>
  </tr>
</tbody>
</table>

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-4)Hacinamiento</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> depto </th>
   <th style="text-align:left;"> mpio </th>
   <th style="text-align:left;"> area </th>
   <th style="text-align:left;"> sexo </th>
   <th style="text-align:left;"> etnia </th>
   <th style="text-align:left;"> anoest </th>
   <th style="text-align:left;"> edad </th>
   <th style="text-align:right;"> condact3 </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> yno </th>
   <th style="text-align:right;"> ysi </th>
   <th style="text-align:right;"> area1 </th>
   <th style="text-align:right;"> sexo2 </th>
   <th style="text-align:right;"> edad2 </th>
   <th style="text-align:right;"> edad3 </th>
   <th style="text-align:right;"> edad4 </th>
   <th style="text-align:right;"> edad5 </th>
   <th style="text-align:right;"> etnia2 </th>
   <th style="text-align:right;"> anoest2 </th>
   <th style="text-align:right;"> anoest3 </th>
   <th style="text-align:right;"> anoest4 </th>
   <th style="text-align:right;"> etnia1 </th>
   <th style="text-align:right;"> tiene_acueducto </th>
   <th style="text-align:right;"> piso_tierra </th>
   <th style="text-align:right;"> alfabeta </th>
   <th style="text-align:right;"> hacinamiento </th>
   <th style="text-align:right;"> tasa_desocupacion </th>
   <th style="text-align:right;"> luces_nocturnas </th>
   <th style="text-align:right;"> cubrimiento_cultivo </th>
   <th style="text-align:right;"> cubrimiento_urbano </th>
   <th style="text-align:right;"> modificacion_humana </th>
   <th style="text-align:right;"> accesibilidad_hospitales </th>
   <th style="text-align:right;"> accesibilidad_hosp_caminado </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1434 </td>
   <td style="text-align:right;"> 342 </td>
   <td style="text-align:right;"> 1092 </td>
   <td style="text-align:right;"> 0.9979 </td>
   <td style="text-align:right;"> 0.5219 </td>
   <td style="text-align:right;"> 0.2690 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.3810 </td>
   <td style="text-align:right;"> 0.2938 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 22.0069 </td>
   <td style="text-align:right;"> 9.1869 </td>
   <td style="text-align:right;"> 19.7751 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 61.3823 </td>
   <td style="text-align:right;"> 259.2423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 47001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1432 </td>
   <td style="text-align:right;"> 657 </td>
   <td style="text-align:right;"> 775 </td>
   <td style="text-align:right;"> 0.9150 </td>
   <td style="text-align:right;"> 0.5158 </td>
   <td style="text-align:right;"> 0.2705 </td>
   <td style="text-align:right;"> 0.2125 </td>
   <td style="text-align:right;"> 0.1913 </td>
   <td style="text-align:right;"> 0.0719 </td>
   <td style="text-align:right;"> 0.0354 </td>
   <td style="text-align:right;"> 0.2539 </td>
   <td style="text-align:right;"> 0.4149 </td>
   <td style="text-align:right;"> 0.1837 </td>
   <td style="text-align:right;"> 0.0169 </td>
   <td style="text-align:right;"> 0.3002 </td>
   <td style="text-align:right;"> 0.0312 </td>
   <td style="text-align:right;"> 0.0364 </td>
   <td style="text-align:right;"> 0.2845 </td>
   <td style="text-align:right;"> 0.0015 </td>
   <td style="text-align:right;"> 4.3364 </td>
   <td style="text-align:right;"> 0.4577 </td>
   <td style="text-align:right;"> 1.7512 </td>
   <td style="text-align:right;"> 0.3278 </td>
   <td style="text-align:right;"> 210.3355 </td>
   <td style="text-align:right;"> 611.8750 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1387 </td>
   <td style="text-align:right;"> 118 </td>
   <td style="text-align:right;"> 1269 </td>
   <td style="text-align:right;"> 0.9979 </td>
   <td style="text-align:right;"> 0.5219 </td>
   <td style="text-align:right;"> 0.2690 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.3810 </td>
   <td style="text-align:right;"> 0.2938 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 22.0069 </td>
   <td style="text-align:right;"> 9.1869 </td>
   <td style="text-align:right;"> 19.7751 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 61.3823 </td>
   <td style="text-align:right;"> 259.2423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 11001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1326 </td>
   <td style="text-align:right;"> 251 </td>
   <td style="text-align:right;"> 1075 </td>
   <td style="text-align:right;"> 0.9979 </td>
   <td style="text-align:right;"> 0.5219 </td>
   <td style="text-align:right;"> 0.2690 </td>
   <td style="text-align:right;"> 0.2316 </td>
   <td style="text-align:right;"> 0.2251 </td>
   <td style="text-align:right;"> 0.0886 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.2098 </td>
   <td style="text-align:right;"> 0.3810 </td>
   <td style="text-align:right;"> 0.2938 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 0.0219 </td>
   <td style="text-align:right;"> 0.0026 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0176 </td>
   <td style="text-align:right;"> 22.0069 </td>
   <td style="text-align:right;"> 9.1869 </td>
   <td style="text-align:right;"> 19.7751 </td>
   <td style="text-align:right;"> 0.5697 </td>
   <td style="text-align:right;"> 61.3823 </td>
   <td style="text-align:right;"> 259.2423 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 08001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1280 </td>
   <td style="text-align:right;"> 583 </td>
   <td style="text-align:right;"> 697 </td>
   <td style="text-align:right;"> 0.9993 </td>
   <td style="text-align:right;"> 0.5208 </td>
   <td style="text-align:right;"> 0.2558 </td>
   <td style="text-align:right;"> 0.2131 </td>
   <td style="text-align:right;"> 0.2146 </td>
   <td style="text-align:right;"> 0.0968 </td>
   <td style="text-align:right;"> 0.0521 </td>
   <td style="text-align:right;"> 0.2324 </td>
   <td style="text-align:right;"> 0.3976 </td>
   <td style="text-align:right;"> 0.2411 </td>
   <td style="text-align:right;"> 0.0012 </td>
   <td style="text-align:right;"> 0.0188 </td>
   <td style="text-align:right;"> 0.0139 </td>
   <td style="text-align:right;"> 0.0232 </td>
   <td style="text-align:right;"> 0.2084 </td>
   <td style="text-align:right;"> 0.0029 </td>
   <td style="text-align:right;"> 58.0251 </td>
   <td style="text-align:right;"> 3.8798 </td>
   <td style="text-align:right;"> 50.9581 </td>
   <td style="text-align:right;"> 0.8114 </td>
   <td style="text-align:right;"> 3.2552 </td>
   <td style="text-align:right;"> 21.7908 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 23001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1236 </td>
   <td style="text-align:right;"> 503 </td>
   <td style="text-align:right;"> 733 </td>
   <td style="text-align:right;"> 0.8192 </td>
   <td style="text-align:right;"> 0.5149 </td>
   <td style="text-align:right;"> 0.2631 </td>
   <td style="text-align:right;"> 0.2146 </td>
   <td style="text-align:right;"> 0.1961 </td>
   <td style="text-align:right;"> 0.0761 </td>
   <td style="text-align:right;"> 0.0170 </td>
   <td style="text-align:right;"> 0.2857 </td>
   <td style="text-align:right;"> 0.3634 </td>
   <td style="text-align:right;"> 0.1920 </td>
   <td style="text-align:right;"> 0.0072 </td>
   <td style="text-align:right;"> 0.1251 </td>
   <td style="text-align:right;"> 0.1412 </td>
   <td style="text-align:right;"> 0.0657 </td>
   <td style="text-align:right;"> 0.2275 </td>
   <td style="text-align:right;"> 0.0010 </td>
   <td style="text-align:right;"> 3.8284 </td>
   <td style="text-align:right;"> 19.0431 </td>
   <td style="text-align:right;"> 1.0209 </td>
   <td style="text-align:right;"> 0.4172 </td>
   <td style="text-align:right;"> 48.4391 </td>
   <td style="text-align:right;"> 234.8668 </td>
  </tr>
</tbody>
</table>

## Definiendo de los modelos en Stan.

Para cada dimensión que compone el IPM se ajusta un modelo mixtos logit Bernoulli estimando mediante técnicas bayesiana. En este código se incluye el uso de la función `future_map` que permite procesar en paralelo cada modelo O puede compilar cada por separado, en nuestro caso se proceso cada modelo por separado.    


```r
library(furrr)
plan(multisession, workers = 4)

fit <- future_map(encuesta_df, function(xdat){
stan_glmer(
  cbind(yno, ysi) ~ (1 | mpio) +
    (1 | depto) +
    edad +
    area +
    anoest +
    etnia +
    sexo + 
    tasa_desocupacion ,
  family = binomial(link = "logit"),
  data = xdat,
  cores = 7,
  chains = 4,
  iter = 300
)}, 
.progress = TRUE)

saveRDS(object = fit, "Data/fits_IPM.rds")
```

Terminado la compilación de los modelos después de realizar validaciones sobre esto, pasamos hacer las predicciones en el censo. 

## Proceso para la predicción $\pi_{di}^{kl}$

Los modelos fueron compilados de manera separada, por tanto, disponemos de un objeto `.rds` por cada dimensión del IPM 


```r
fit_agua <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_agua.rds")
fit_educacion <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_educacion.rds")
fit_empleo <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_empleo.rds")
fit_energia <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_Energia.rds")
fit_hacinamiento <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_Hacinamiento.rds")
fit_internet <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_internet.rds")
fit_material <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_material.rds")
fit_saneamiento <-
  readRDS(file = "Modelo_bayes_HxA/COL/Data/fit_bayes_saneamiento.rds")
```


Ahora, debemos leer la información del censo  y crear los **post-estrato **

```r
censo_ipm <- readRDS("Modelo_bayes_HxA/COL/Data/censo_COL.rds") %>%
  rename(dam = depto, dam2 = mpio) %>%
  group_by(dam,   dam2,  area,  sexo,  edad,  etnia, anoest) %>%
  summarise(n = sum(n), .groups = "drop")

statelevel_predictors_df <-
  readRDS("Modelo_bayes_HxA/COL/Data/statelevel_predictors_df_dam2.rds") 
```

Para realizar la predicción en el censo debemos incluir la información auxiliar 


```r
poststrat_df <- left_join(censo_ipm, statelevel_predictors_df,
                          by = c("dam", "dam2")) %>% 
  rename(depto = dam,  mpio = dam2) 
```


Para cada uno de los modelos anteriores debe tener las $L$ predicciones de $\pi_{di}^{kl}$,

-   Privación de acceso al agua potable. 

```r
temp <- poststrat_df 
epred_mat_agua <- posterior_epred(
  fit_agua,
  newdata = temp,
  type = "response",
  allow.new.levels = TRUE
)
```

-   Privación de la educación.


```r
epred_mat_educacion <-
  posterior_epred(
    fit_educacion,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```

-   Privación del empleo y la protección social.


```r
epred_mat_empleo <-
  posterior_epred(
    fit_empleo,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```

-   Acceso al servicio energía eléctrica.


```r
epred_mat_energia <-
  posterior_epred(
    fit_energia,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```

-    Hacinamiento en el hogar.


```r
epred_mat_hacinamiento <-
  posterior_epred(
    fit_hacinamiento,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```

-   Acceso al servicio de Internet.


```r
epred_mat_internet <-
  posterior_epred(
    fit_internet,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```

-   Privación en material de construcción de la vivienda


```r
epred_mat_material <-
  posterior_epred(
    fit_material,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```

-   Privación en saneamiento.


```r
epred_mat_saneamiento <-
  posterior_epred(
    fit_saneamiento,
    newdata = temp,
    type = "response",
    allow.new.levels = TRUE
  )
```



## Definiendo los hard estimates

Los resultados anteriores se deben procesarse para obtener los hard estimates, es decir, términos de carencia (1) y  no carencia (0) para la $k-esima$ dimensión . 

-    Privación de acceso al agua potable. 



```r
epred_mat_agua_dummy <-
  rbinom(n = nrow(epred_mat_agua) * ncol(epred_mat_agua) , 1,
         epred_mat_agua)

epred_mat_agua_dummy <- matrix(
  epred_mat_agua_dummy,
  nrow = nrow(epred_mat_agua),
  ncol = ncol(epred_mat_agua)
)
```

-   Privación de la educación.



```r
epred_mat_educacion_dummy <-
  rbinom(n = nrow(epred_mat_educacion) * ncol(epred_mat_educacion) ,
         1,
         epred_mat_educacion)

epred_mat_educacion_dummy <- matrix(
  epred_mat_educacion_dummy,
  nrow = nrow(epred_mat_educacion),
  ncol = ncol(epred_mat_educacion)
)
```

-    Acceso al servicio energía eléctrica 



```r
epred_mat_energia_dummy <-
  rbinom(n = nrow(epred_mat_energia) * ncol(epred_mat_energia) ,
         1,
         epred_mat_energia)

epred_mat_energia_dummy <- matrix(
  epred_mat_energia_dummy,
  nrow = nrow(epred_mat_energia),
  ncol = ncol(epred_mat_energia)
)
```

-   Hacinamiento en el hogar.



```r
epred_mat_hacinamiento_dummy <-
  rbinom(
    n = nrow(epred_mat_hacinamiento) * ncol(epred_mat_hacinamiento) ,
    1,
    epred_mat_hacinamiento
  )

epred_mat_hacinamiento_dummy <-
  matrix(
    epred_mat_hacinamiento_dummy,
    nrow = nrow(epred_mat_hacinamiento),
    ncol = ncol(epred_mat_hacinamiento)
  )
```

-   Acceso al servicio de Internet.



```r
epred_mat_internet_dummy <-
  rbinom(n = nrow(epred_mat_internet) * ncol(epred_mat_internet) ,
         1,
         epred_mat_internet)

epred_mat_internet_dummy <- matrix(
  epred_mat_internet_dummy,
  nrow = nrow(epred_mat_internet),
  ncol = ncol(epred_mat_internet)
)
```

-   Privación en material de construcción de la vivienda 



```r
epred_mat_material_dummy <-
  rbinom(n = nrow(epred_mat_material) * ncol(epred_mat_material) ,
         1,
         epred_mat_material)

epred_mat_material_dummy <- matrix(
  epred_mat_material_dummy,
  nrow = nrow(epred_mat_material),
  ncol = ncol(epred_mat_material)
)
```

-   Privación en saneamiento. 



```r
epred_mat_saneamiento_dummy <-
  rbinom(n = nrow(epred_mat_saneamiento) * ncol(epred_mat_saneamiento) ,
         1,
         epred_mat_saneamiento)

epred_mat_saneamiento_dummy <- matrix(
  epred_mat_saneamiento_dummy,
  nrow = nrow(epred_mat_saneamiento),
  ncol = ncol(epred_mat_saneamiento)
)
```

-   Privación del empleo y la protección social. 



```r
epred_mat_empleo_dummy <-
  rbinom(n = nrow(epred_mat_empleo) * ncol(epred_mat_empleo) ,
         1,
         epred_mat_empleo)

epred_mat_empleo_dummy <- matrix(
  epred_mat_empleo_dummy,
  nrow = nrow(epred_mat_empleo),
  ncol = ncol(epred_mat_empleo)
)
```




## Calculando $q_{di}^{l}$, $I\left( q_{di}^{l} > z \right)$ y $c_{di}^{l}\left(z\right)$

Dado que los hard estimates fueron organizados en matrices, el calculo de $q^{l}_{id}$ es una simple operación matricial la cual se muestra a continuación 

```r
chain_q  <- 0.1 * (
  epred_mat_material_dummy +
    epred_mat_hacinamiento_dummy +
    epred_mat_agua_dummy +
    epred_mat_saneamiento_dummy +
    epred_mat_energia_dummy + epred_mat_internet_dummy
) +
  0.2 * (epred_mat_educacion_dummy +
           epred_mat_empleo_dummy)
```



Ahora, es posible tener el calculo de $I\left( q_{di}^{l} > z \right)$, tomando como umbral $z=0.4$. 


```r
chain_Ind <- chain_q
chain_Ind[chain_Ind <= 0.4] <- 0
chain_Ind[chain_Ind != 0] <- 1
```

seguidamente calculamos $c_{di}^{l}\left(z\right)$ 


```r
chain_ci <- matrix(0,nrow = nrow(chain_q), ncol = ncol(chain_q))
chain_ci[chain_Ind == 1] <- chain_q[chain_Ind == 1]
```



```r
datos<-data.frame(t(chain_q[1:5,1:10]), t(chain_Ind[1:5,1:10]), t(chain_ci[1:5,1:10]),
                  N = censo_ipm$n[1:10] )
colnames(datos) <- c(paste0("q",1:5), paste0("Ind",1:5),paste0("c",1:5),"N")
tba(datos, "Cadenas obtenidas")
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-31)Cadenas obtenidas</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> q1 </th>
   <th style="text-align:right;"> q2 </th>
   <th style="text-align:right;"> q3 </th>
   <th style="text-align:right;"> q4 </th>
   <th style="text-align:right;"> q5 </th>
   <th style="text-align:right;"> Ind1 </th>
   <th style="text-align:right;"> Ind2 </th>
   <th style="text-align:right;"> Ind3 </th>
   <th style="text-align:right;"> Ind4 </th>
   <th style="text-align:right;"> Ind5 </th>
   <th style="text-align:right;"> c1 </th>
   <th style="text-align:right;"> c2 </th>
   <th style="text-align:right;"> c3 </th>
   <th style="text-align:right;"> c4 </th>
   <th style="text-align:right;"> c5 </th>
   <th style="text-align:right;"> N </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 22 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 76 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 796 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.0 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 3549 </td>
  </tr>
</tbody>
</table>

## Estimación de $H$, $A$ e $IPM$
Para este proceso debemos realizar sumas ponderadas, dado que cada registro de la base de datos representa  un grupo de observaciones con las mismas características.  


```r
numIPM <- t(chain_ci) %>%
  as.data.frame() %>%
  mutate_all(~ . * censo_ipm$n) %>%
  as.matrix()

chain_N <- t(chain_Ind) %>%
  as.data.frame() %>%
  mutate_all(~ . * censo_ipm$n) %>%
  as.matrix()


IPM_l <- colSums(numIPM)/sum(censo_ipm$n)
Nz_l <- colSums(chain_N)
H_l <- Nz_l/sum(censo_ipm$n)
A_l <- colSums(numIPM)/Nz_l
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-33)l-iteraciones</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> IPM_l </th>
   <th style="text-align:right;"> Nz_l </th>
   <th style="text-align:right;"> H_l </th>
   <th style="text-align:right;"> A_l </th>
   <th style="text-align:right;"> HA_l </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> l = 1 </td>
   <td style="text-align:right;"> 0.1902 </td>
   <td style="text-align:right;"> 10996210 </td>
   <td style="text-align:right;"> 0.3217 </td>
   <td style="text-align:right;"> 0.5911 </td>
   <td style="text-align:right;"> 0.1902 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 2 </td>
   <td style="text-align:right;"> 0.2154 </td>
   <td style="text-align:right;"> 12202627 </td>
   <td style="text-align:right;"> 0.3570 </td>
   <td style="text-align:right;"> 0.6032 </td>
   <td style="text-align:right;"> 0.2154 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 3 </td>
   <td style="text-align:right;"> 0.2019 </td>
   <td style="text-align:right;"> 11414502 </td>
   <td style="text-align:right;"> 0.3339 </td>
   <td style="text-align:right;"> 0.6045 </td>
   <td style="text-align:right;"> 0.2019 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 4 </td>
   <td style="text-align:right;"> 0.2003 </td>
   <td style="text-align:right;"> 11515260 </td>
   <td style="text-align:right;"> 0.3369 </td>
   <td style="text-align:right;"> 0.5947 </td>
   <td style="text-align:right;"> 0.2003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 5 </td>
   <td style="text-align:right;"> 0.2024 </td>
   <td style="text-align:right;"> 11679743 </td>
   <td style="text-align:right;"> 0.3417 </td>
   <td style="text-align:right;"> 0.5924 </td>
   <td style="text-align:right;"> 0.2024 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 6 </td>
   <td style="text-align:right;"> 0.1914 </td>
   <td style="text-align:right;"> 10856902 </td>
   <td style="text-align:right;"> 0.3176 </td>
   <td style="text-align:right;"> 0.6025 </td>
   <td style="text-align:right;"> 0.1914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 7 </td>
   <td style="text-align:right;"> 0.1970 </td>
   <td style="text-align:right;"> 11325242 </td>
   <td style="text-align:right;"> 0.3313 </td>
   <td style="text-align:right;"> 0.5944 </td>
   <td style="text-align:right;"> 0.1970 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 8 </td>
   <td style="text-align:right;"> 0.1967 </td>
   <td style="text-align:right;"> 11309073 </td>
   <td style="text-align:right;"> 0.3309 </td>
   <td style="text-align:right;"> 0.5945 </td>
   <td style="text-align:right;"> 0.1967 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 9 </td>
   <td style="text-align:right;"> 0.1966 </td>
   <td style="text-align:right;"> 11325920 </td>
   <td style="text-align:right;"> 0.3314 </td>
   <td style="text-align:right;"> 0.5935 </td>
   <td style="text-align:right;"> 0.1966 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> l = 10 </td>
   <td style="text-align:right;"> 0.2223 </td>
   <td style="text-align:right;"> 12466527 </td>
   <td style="text-align:right;"> 0.3647 </td>
   <td style="text-align:right;"> 0.6094 </td>
   <td style="text-align:right;"> 0.2223 </td>
  </tr>
</tbody>
</table>


Por último se realiza las estimaciones puntuales y varianza para $H$, $A$ y $IPM$, esto es:  


```r
estimacion <- data.frame(H = mean(H_l),
           H_sd = sd(H_l),
           A = mean(A_l),
           A_sd = sd(A_l),
           IPM = mean(IPM_l),
           IPM_sd = sd(IPM_l))
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-35)Estimaciones Nacionales</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> H </th>
   <th style="text-align:right;"> H_sd </th>
   <th style="text-align:right;"> A </th>
   <th style="text-align:right;"> A_sd </th>
   <th style="text-align:right;"> IPM </th>
   <th style="text-align:right;"> IPM_sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.3304 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 0.5939 </td>
   <td style="text-align:right;"> 0.0068 </td>
   <td style="text-align:right;"> 0.1963 </td>
   <td style="text-align:right;"> 0.0104 </td>
  </tr>
</tbody>
</table>
## Estimaciones desagregadas del IPM

Para realizar las estimaciones desagregadas se desarrollo una función que facilita el calculo, la estructura general el proceso es repetir el proceso anterior por subgrupos, por ejemplo, departamento (*dam*)


```r
source("Modelo_bayes_HxA/0funciones/Estimar_ipm.R")
ipm_dam <- estime_IPM(poststrat = censo_ipm,
                      chain_ci = chain_ci,
                      chain_ind = chain_ind,
           byMap = "dam"
           ) %>% data.frame()
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-37)Estimaciones por departamento</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam </th>
   <th style="text-align:right;"> H </th>
   <th style="text-align:right;"> H_sd </th>
   <th style="text-align:right;"> A </th>
   <th style="text-align:right;"> A_sd </th>
   <th style="text-align:right;"> IPM </th>
   <th style="text-align:right;"> IPM_sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:right;"> 0.2651 </td>
   <td style="text-align:right;"> 0.0373 </td>
   <td style="text-align:right;"> 0.5731 </td>
   <td style="text-align:right;"> 0.0117 </td>
   <td style="text-align:right;"> 0.1518 </td>
   <td style="text-align:right;"> 0.0206 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:right;"> 0.2438 </td>
   <td style="text-align:right;"> 0.0423 </td>
   <td style="text-align:right;"> 0.5496 </td>
   <td style="text-align:right;"> 0.0115 </td>
   <td style="text-align:right;"> 0.1339 </td>
   <td style="text-align:right;"> 0.0228 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:right;"> 0.0978 </td>
   <td style="text-align:right;"> 0.0590 </td>
   <td style="text-align:right;"> 0.5204 </td>
   <td style="text-align:right;"> 0.0234 </td>
   <td style="text-align:right;"> 0.0507 </td>
   <td style="text-align:right;"> 0.0305 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:right;"> 0.4637 </td>
   <td style="text-align:right;"> 0.0363 </td>
   <td style="text-align:right;"> 0.6131 </td>
   <td style="text-align:right;"> 0.0119 </td>
   <td style="text-align:right;"> 0.2843 </td>
   <td style="text-align:right;"> 0.0227 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:right;"> 0.4041 </td>
   <td style="text-align:right;"> 0.0250 </td>
   <td style="text-align:right;"> 0.5854 </td>
   <td style="text-align:right;"> 0.0089 </td>
   <td style="text-align:right;"> 0.2367 </td>
   <td style="text-align:right;"> 0.0166 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:right;"> 0.2765 </td>
   <td style="text-align:right;"> 0.0273 </td>
   <td style="text-align:right;"> 0.5734 </td>
   <td style="text-align:right;"> 0.0079 </td>
   <td style="text-align:right;"> 0.1585 </td>
   <td style="text-align:right;"> 0.0150 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:right;"> 0.5534 </td>
   <td style="text-align:right;"> 0.0447 </td>
   <td style="text-align:right;"> 0.6105 </td>
   <td style="text-align:right;"> 0.0088 </td>
   <td style="text-align:right;"> 0.3377 </td>
   <td style="text-align:right;"> 0.0256 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:right;"> 0.5790 </td>
   <td style="text-align:right;"> 0.0233 </td>
   <td style="text-align:right;"> 0.6201 </td>
   <td style="text-align:right;"> 0.0066 </td>
   <td style="text-align:right;"> 0.3591 </td>
   <td style="text-align:right;"> 0.0156 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:right;"> 0.4640 </td>
   <td style="text-align:right;"> 0.0439 </td>
   <td style="text-align:right;"> 0.5974 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 0.2772 </td>
   <td style="text-align:right;"> 0.0268 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:right;"> 0.5960 </td>
   <td style="text-align:right;"> 0.0267 </td>
   <td style="text-align:right;"> 0.6461 </td>
   <td style="text-align:right;"> 0.0090 </td>
   <td style="text-align:right;"> 0.3850 </td>
   <td style="text-align:right;"> 0.0166 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:right;"> 0.3134 </td>
   <td style="text-align:right;"> 0.0360 </td>
   <td style="text-align:right;"> 0.5762 </td>
   <td style="text-align:right;"> 0.0105 </td>
   <td style="text-align:right;"> 0.1807 </td>
   <td style="text-align:right;"> 0.0217 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:right;"> 0.7703 </td>
   <td style="text-align:right;"> 0.0340 </td>
   <td style="text-align:right;"> 0.6313 </td>
   <td style="text-align:right;"> 0.0081 </td>
   <td style="text-align:right;"> 0.4863 </td>
   <td style="text-align:right;"> 0.0226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:right;"> 0.4421 </td>
   <td style="text-align:right;"> 0.0320 </td>
   <td style="text-align:right;"> 0.5846 </td>
   <td style="text-align:right;"> 0.0128 </td>
   <td style="text-align:right;"> 0.2585 </td>
   <td style="text-align:right;"> 0.0201 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:right;"> 0.7391 </td>
   <td style="text-align:right;"> 0.0254 </td>
   <td style="text-align:right;"> 0.6965 </td>
   <td style="text-align:right;"> 0.0081 </td>
   <td style="text-align:right;"> 0.5147 </td>
   <td style="text-align:right;"> 0.0172 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:right;"> 0.5081 </td>
   <td style="text-align:right;"> 0.0458 </td>
   <td style="text-align:right;"> 0.6099 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 0.3098 </td>
   <td style="text-align:right;"> 0.0275 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:right;"> 0.3607 </td>
   <td style="text-align:right;"> 0.0437 </td>
   <td style="text-align:right;"> 0.5920 </td>
   <td style="text-align:right;"> 0.0128 </td>
   <td style="text-align:right;"> 0.2133 </td>
   <td style="text-align:right;"> 0.0238 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:right;"> 0.6008 </td>
   <td style="text-align:right;"> 0.0316 </td>
   <td style="text-align:right;"> 0.6170 </td>
   <td style="text-align:right;"> 0.0075 </td>
   <td style="text-align:right;"> 0.3707 </td>
   <td style="text-align:right;"> 0.0199 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:right;"> 0.4295 </td>
   <td style="text-align:right;"> 0.0456 </td>
   <td style="text-align:right;"> 0.5866 </td>
   <td style="text-align:right;"> 0.0102 </td>
   <td style="text-align:right;"> 0.2518 </td>
   <td style="text-align:right;"> 0.0257 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:right;"> 0.2303 </td>
   <td style="text-align:right;"> 0.0414 </td>
   <td style="text-align:right;"> 0.5473 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 0.1259 </td>
   <td style="text-align:right;"> 0.0218 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:right;"> 0.2677 </td>
   <td style="text-align:right;"> 0.0368 </td>
   <td style="text-align:right;"> 0.5652 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 0.1510 </td>
   <td style="text-align:right;"> 0.0193 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:right;"> 0.3225 </td>
   <td style="text-align:right;"> 0.0246 </td>
   <td style="text-align:right;"> 0.5898 </td>
   <td style="text-align:right;"> 0.0102 </td>
   <td style="text-align:right;"> 0.1902 </td>
   <td style="text-align:right;"> 0.0146 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:right;"> 0.5707 </td>
   <td style="text-align:right;"> 0.0344 </td>
   <td style="text-align:right;"> 0.6260 </td>
   <td style="text-align:right;"> 0.0115 </td>
   <td style="text-align:right;"> 0.3573 </td>
   <td style="text-align:right;"> 0.0232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 73 </td>
   <td style="text-align:right;"> 0.3954 </td>
   <td style="text-align:right;"> 0.0310 </td>
   <td style="text-align:right;"> 0.5891 </td>
   <td style="text-align:right;"> 0.0123 </td>
   <td style="text-align:right;"> 0.2329 </td>
   <td style="text-align:right;"> 0.0192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:right;"> 0.2052 </td>
   <td style="text-align:right;"> 0.0232 </td>
   <td style="text-align:right;"> 0.5552 </td>
   <td style="text-align:right;"> 0.0100 </td>
   <td style="text-align:right;"> 0.1139 </td>
   <td style="text-align:right;"> 0.0125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 81 </td>
   <td style="text-align:right;"> 0.5829 </td>
   <td style="text-align:right;"> 0.0998 </td>
   <td style="text-align:right;"> 0.5991 </td>
   <td style="text-align:right;"> 0.0271 </td>
   <td style="text-align:right;"> 0.3509 </td>
   <td style="text-align:right;"> 0.0706 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:right;"> 0.5409 </td>
   <td style="text-align:right;"> 0.1040 </td>
   <td style="text-align:right;"> 0.5924 </td>
   <td style="text-align:right;"> 0.0270 </td>
   <td style="text-align:right;"> 0.3220 </td>
   <td style="text-align:right;"> 0.0707 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 86 </td>
   <td style="text-align:right;"> 0.6122 </td>
   <td style="text-align:right;"> 0.0940 </td>
   <td style="text-align:right;"> 0.6059 </td>
   <td style="text-align:right;"> 0.0277 </td>
   <td style="text-align:right;"> 0.3728 </td>
   <td style="text-align:right;"> 0.0702 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 88 </td>
   <td style="text-align:right;"> 0.4691 </td>
   <td style="text-align:right;"> 0.1161 </td>
   <td style="text-align:right;"> 0.5793 </td>
   <td style="text-align:right;"> 0.0252 </td>
   <td style="text-align:right;"> 0.2732 </td>
   <td style="text-align:right;"> 0.0735 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 91 </td>
   <td style="text-align:right;"> 0.6110 </td>
   <td style="text-align:right;"> 0.1082 </td>
   <td style="text-align:right;"> 0.6159 </td>
   <td style="text-align:right;"> 0.0289 </td>
   <td style="text-align:right;"> 0.3782 </td>
   <td style="text-align:right;"> 0.0786 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 94 </td>
   <td style="text-align:right;"> 0.6954 </td>
   <td style="text-align:right;"> 0.0919 </td>
   <td style="text-align:right;"> 0.6304 </td>
   <td style="text-align:right;"> 0.0331 </td>
   <td style="text-align:right;"> 0.4402 </td>
   <td style="text-align:right;"> 0.0739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 95 </td>
   <td style="text-align:right;"> 0.5962 </td>
   <td style="text-align:right;"> 0.1044 </td>
   <td style="text-align:right;"> 0.6044 </td>
   <td style="text-align:right;"> 0.0276 </td>
   <td style="text-align:right;"> 0.3617 </td>
   <td style="text-align:right;"> 0.0724 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 97 </td>
   <td style="text-align:right;"> 0.7010 </td>
   <td style="text-align:right;"> 0.0996 </td>
   <td style="text-align:right;"> 0.6355 </td>
   <td style="text-align:right;"> 0.0352 </td>
   <td style="text-align:right;"> 0.4475 </td>
   <td style="text-align:right;"> 0.0797 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:right;"> 0.7090 </td>
   <td style="text-align:right;"> 0.0831 </td>
   <td style="text-align:right;"> 0.6355 </td>
   <td style="text-align:right;"> 0.0350 </td>
   <td style="text-align:right;"> 0.4524 </td>
   <td style="text-align:right;"> 0.0708 </td>
  </tr>
</tbody>
</table>

Otra estimación desagregada que es posible obtener es la combinación por departamento y sexo, para ellos se usa la sintaxis. 



```r
ipm_dam_sexo <- estime_IPM(
  poststrat = censo_ipm,
  chain_ci = chain_ci,
  chain_ind = chain_ind,
  byMap = c("dam", "sexo")
) %>% data.frame()
```





<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-40)Estimaciones por departamento y sexo</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam </th>
   <th style="text-align:left;"> sexo </th>
   <th style="text-align:right;"> H </th>
   <th style="text-align:right;"> H_sd </th>
   <th style="text-align:right;"> A </th>
   <th style="text-align:right;"> A_sd </th>
   <th style="text-align:right;"> IPM </th>
   <th style="text-align:right;"> IPM_sd </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2714 </td>
   <td style="text-align:right;"> 0.0520 </td>
   <td style="text-align:right;"> 0.5772 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 0.1563 </td>
   <td style="text-align:right;"> 0.0283 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2595 </td>
   <td style="text-align:right;"> 0.0444 </td>
   <td style="text-align:right;"> 0.5701 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 0.1477 </td>
   <td style="text-align:right;"> 0.0246 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2486 </td>
   <td style="text-align:right;"> 0.0660 </td>
   <td style="text-align:right;"> 0.5514 </td>
   <td style="text-align:right;"> 0.0164 </td>
   <td style="text-align:right;"> 0.1367 </td>
   <td style="text-align:right;"> 0.0352 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2394 </td>
   <td style="text-align:right;"> 0.0561 </td>
   <td style="text-align:right;"> 0.5494 </td>
   <td style="text-align:right;"> 0.0165 </td>
   <td style="text-align:right;"> 0.1313 </td>
   <td style="text-align:right;"> 0.0301 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.0926 </td>
   <td style="text-align:right;"> 0.0855 </td>
   <td style="text-align:right;"> 0.5274 </td>
   <td style="text-align:right;"> 0.0326 </td>
   <td style="text-align:right;"> 0.0487 </td>
   <td style="text-align:right;"> 0.0453 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.1024 </td>
   <td style="text-align:right;"> 0.0853 </td>
   <td style="text-align:right;"> 0.5227 </td>
   <td style="text-align:right;"> 0.0304 </td>
   <td style="text-align:right;"> 0.0525 </td>
   <td style="text-align:right;"> 0.0431 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4680 </td>
   <td style="text-align:right;"> 0.0486 </td>
   <td style="text-align:right;"> 0.6202 </td>
   <td style="text-align:right;"> 0.0149 </td>
   <td style="text-align:right;"> 0.2900 </td>
   <td style="text-align:right;"> 0.0289 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.4595 </td>
   <td style="text-align:right;"> 0.0509 </td>
   <td style="text-align:right;"> 0.6070 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 0.2787 </td>
   <td style="text-align:right;"> 0.0295 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4094 </td>
   <td style="text-align:right;"> 0.0308 </td>
   <td style="text-align:right;"> 0.5887 </td>
   <td style="text-align:right;"> 0.0094 </td>
   <td style="text-align:right;"> 0.2411 </td>
   <td style="text-align:right;"> 0.0195 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.3992 </td>
   <td style="text-align:right;"> 0.0296 </td>
   <td style="text-align:right;"> 0.5824 </td>
   <td style="text-align:right;"> 0.0098 </td>
   <td style="text-align:right;"> 0.2325 </td>
   <td style="text-align:right;"> 0.0185 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2900 </td>
   <td style="text-align:right;"> 0.0406 </td>
   <td style="text-align:right;"> 0.5761 </td>
   <td style="text-align:right;"> 0.0112 </td>
   <td style="text-align:right;"> 0.1668 </td>
   <td style="text-align:right;"> 0.0215 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2641 </td>
   <td style="text-align:right;"> 0.0384 </td>
   <td style="text-align:right;"> 0.5715 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 0.1508 </td>
   <td style="text-align:right;"> 0.0210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5520 </td>
   <td style="text-align:right;"> 0.0630 </td>
   <td style="text-align:right;"> 0.6153 </td>
   <td style="text-align:right;"> 0.0120 </td>
   <td style="text-align:right;"> 0.3393 </td>
   <td style="text-align:right;"> 0.0358 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5549 </td>
   <td style="text-align:right;"> 0.0608 </td>
   <td style="text-align:right;"> 0.6064 </td>
   <td style="text-align:right;"> 0.0131 </td>
   <td style="text-align:right;"> 0.3361 </td>
   <td style="text-align:right;"> 0.0337 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5874 </td>
   <td style="text-align:right;"> 0.0316 </td>
   <td style="text-align:right;"> 0.6214 </td>
   <td style="text-align:right;"> 0.0087 </td>
   <td style="text-align:right;"> 0.3650 </td>
   <td style="text-align:right;"> 0.0195 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5710 </td>
   <td style="text-align:right;"> 0.0322 </td>
   <td style="text-align:right;"> 0.6189 </td>
   <td style="text-align:right;"> 0.0078 </td>
   <td style="text-align:right;"> 0.3534 </td>
   <td style="text-align:right;"> 0.0203 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4671 </td>
   <td style="text-align:right;"> 0.0544 </td>
   <td style="text-align:right;"> 0.6033 </td>
   <td style="text-align:right;"> 0.0132 </td>
   <td style="text-align:right;"> 0.2816 </td>
   <td style="text-align:right;"> 0.0318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.4610 </td>
   <td style="text-align:right;"> 0.0565 </td>
   <td style="text-align:right;"> 0.5924 </td>
   <td style="text-align:right;"> 0.0135 </td>
   <td style="text-align:right;"> 0.2729 </td>
   <td style="text-align:right;"> 0.0327 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.6003 </td>
   <td style="text-align:right;"> 0.0362 </td>
   <td style="text-align:right;"> 0.6508 </td>
   <td style="text-align:right;"> 0.0101 </td>
   <td style="text-align:right;"> 0.3905 </td>
   <td style="text-align:right;"> 0.0221 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5918 </td>
   <td style="text-align:right;"> 0.0382 </td>
   <td style="text-align:right;"> 0.6418 </td>
   <td style="text-align:right;"> 0.0113 </td>
   <td style="text-align:right;"> 0.3796 </td>
   <td style="text-align:right;"> 0.0221 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.3208 </td>
   <td style="text-align:right;"> 0.0450 </td>
   <td style="text-align:right;"> 0.5798 </td>
   <td style="text-align:right;"> 0.0121 </td>
   <td style="text-align:right;"> 0.1860 </td>
   <td style="text-align:right;"> 0.0263 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.3064 </td>
   <td style="text-align:right;"> 0.0409 </td>
   <td style="text-align:right;"> 0.5731 </td>
   <td style="text-align:right;"> 0.0118 </td>
   <td style="text-align:right;"> 0.1756 </td>
   <td style="text-align:right;"> 0.0237 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.7750 </td>
   <td style="text-align:right;"> 0.0435 </td>
   <td style="text-align:right;"> 0.6312 </td>
   <td style="text-align:right;"> 0.0104 </td>
   <td style="text-align:right;"> 0.4891 </td>
   <td style="text-align:right;"> 0.0272 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7658 </td>
   <td style="text-align:right;"> 0.0468 </td>
   <td style="text-align:right;"> 0.6316 </td>
   <td style="text-align:right;"> 0.0116 </td>
   <td style="text-align:right;"> 0.4837 </td>
   <td style="text-align:right;"> 0.0312 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4564 </td>
   <td style="text-align:right;"> 0.0431 </td>
   <td style="text-align:right;"> 0.5876 </td>
   <td style="text-align:right;"> 0.0140 </td>
   <td style="text-align:right;"> 0.2682 </td>
   <td style="text-align:right;"> 0.0265 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.4281 </td>
   <td style="text-align:right;"> 0.0420 </td>
   <td style="text-align:right;"> 0.5818 </td>
   <td style="text-align:right;"> 0.0141 </td>
   <td style="text-align:right;"> 0.2490 </td>
   <td style="text-align:right;"> 0.0243 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.7357 </td>
   <td style="text-align:right;"> 0.0310 </td>
   <td style="text-align:right;"> 0.6964 </td>
   <td style="text-align:right;"> 0.0118 </td>
   <td style="text-align:right;"> 0.5123 </td>
   <td style="text-align:right;"> 0.0232 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7423 </td>
   <td style="text-align:right;"> 0.0327 </td>
   <td style="text-align:right;"> 0.6966 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 0.5169 </td>
   <td style="text-align:right;"> 0.0208 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5107 </td>
   <td style="text-align:right;"> 0.0553 </td>
   <td style="text-align:right;"> 0.6170 </td>
   <td style="text-align:right;"> 0.0143 </td>
   <td style="text-align:right;"> 0.3150 </td>
   <td style="text-align:right;"> 0.0336 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5056 </td>
   <td style="text-align:right;"> 0.0611 </td>
   <td style="text-align:right;"> 0.6037 </td>
   <td style="text-align:right;"> 0.0165 </td>
   <td style="text-align:right;"> 0.3048 </td>
   <td style="text-align:right;"> 0.0337 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.3772 </td>
   <td style="text-align:right;"> 0.0552 </td>
   <td style="text-align:right;"> 0.5966 </td>
   <td style="text-align:right;"> 0.0149 </td>
   <td style="text-align:right;"> 0.2246 </td>
   <td style="text-align:right;"> 0.0300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.3440 </td>
   <td style="text-align:right;"> 0.0589 </td>
   <td style="text-align:right;"> 0.5878 </td>
   <td style="text-align:right;"> 0.0155 </td>
   <td style="text-align:right;"> 0.2018 </td>
   <td style="text-align:right;"> 0.0322 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5936 </td>
   <td style="text-align:right;"> 0.0338 </td>
   <td style="text-align:right;"> 0.6198 </td>
   <td style="text-align:right;"> 0.0093 </td>
   <td style="text-align:right;"> 0.3679 </td>
   <td style="text-align:right;"> 0.0208 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.6075 </td>
   <td style="text-align:right;"> 0.0438 </td>
   <td style="text-align:right;"> 0.6146 </td>
   <td style="text-align:right;"> 0.0087 </td>
   <td style="text-align:right;"> 0.3733 </td>
   <td style="text-align:right;"> 0.0265 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4367 </td>
   <td style="text-align:right;"> 0.0736 </td>
   <td style="text-align:right;"> 0.5908 </td>
   <td style="text-align:right;"> 0.0145 </td>
   <td style="text-align:right;"> 0.2576 </td>
   <td style="text-align:right;"> 0.0412 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.4226 </td>
   <td style="text-align:right;"> 0.0584 </td>
   <td style="text-align:right;"> 0.5833 </td>
   <td style="text-align:right;"> 0.0136 </td>
   <td style="text-align:right;"> 0.2463 </td>
   <td style="text-align:right;"> 0.0333 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2365 </td>
   <td style="text-align:right;"> 0.0569 </td>
   <td style="text-align:right;"> 0.5507 </td>
   <td style="text-align:right;"> 0.0133 </td>
   <td style="text-align:right;"> 0.1300 </td>
   <td style="text-align:right;"> 0.0304 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2248 </td>
   <td style="text-align:right;"> 0.0572 </td>
   <td style="text-align:right;"> 0.5451 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 0.1222 </td>
   <td style="text-align:right;"> 0.0299 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2671 </td>
   <td style="text-align:right;"> 0.0465 </td>
   <td style="text-align:right;"> 0.5704 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 0.1519 </td>
   <td style="text-align:right;"> 0.0242 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2682 </td>
   <td style="text-align:right;"> 0.0559 </td>
   <td style="text-align:right;"> 0.5623 </td>
   <td style="text-align:right;"> 0.0153 </td>
   <td style="text-align:right;"> 0.1503 </td>
   <td style="text-align:right;"> 0.0290 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.3359 </td>
   <td style="text-align:right;"> 0.0357 </td>
   <td style="text-align:right;"> 0.5953 </td>
   <td style="text-align:right;"> 0.0137 </td>
   <td style="text-align:right;"> 0.1998 </td>
   <td style="text-align:right;"> 0.0198 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.3099 </td>
   <td style="text-align:right;"> 0.0342 </td>
   <td style="text-align:right;"> 0.5851 </td>
   <td style="text-align:right;"> 0.0107 </td>
   <td style="text-align:right;"> 0.1812 </td>
   <td style="text-align:right;"> 0.0192 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5834 </td>
   <td style="text-align:right;"> 0.0447 </td>
   <td style="text-align:right;"> 0.6319 </td>
   <td style="text-align:right;"> 0.0130 </td>
   <td style="text-align:right;"> 0.3685 </td>
   <td style="text-align:right;"> 0.0284 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5580 </td>
   <td style="text-align:right;"> 0.0424 </td>
   <td style="text-align:right;"> 0.6203 </td>
   <td style="text-align:right;"> 0.0133 </td>
   <td style="text-align:right;"> 0.3461 </td>
   <td style="text-align:right;"> 0.0263 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 73 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4064 </td>
   <td style="text-align:right;"> 0.0440 </td>
   <td style="text-align:right;"> 0.5945 </td>
   <td style="text-align:right;"> 0.0147 </td>
   <td style="text-align:right;"> 0.2415 </td>
   <td style="text-align:right;"> 0.0253 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 73 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.3847 </td>
   <td style="text-align:right;"> 0.0452 </td>
   <td style="text-align:right;"> 0.5845 </td>
   <td style="text-align:right;"> 0.0138 </td>
   <td style="text-align:right;"> 0.2247 </td>
   <td style="text-align:right;"> 0.0258 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.2069 </td>
   <td style="text-align:right;"> 0.0370 </td>
   <td style="text-align:right;"> 0.5580 </td>
   <td style="text-align:right;"> 0.0117 </td>
   <td style="text-align:right;"> 0.1153 </td>
   <td style="text-align:right;"> 0.0202 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.2037 </td>
   <td style="text-align:right;"> 0.0384 </td>
   <td style="text-align:right;"> 0.5535 </td>
   <td style="text-align:right;"> 0.0126 </td>
   <td style="text-align:right;"> 0.1126 </td>
   <td style="text-align:right;"> 0.0206 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 81 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5858 </td>
   <td style="text-align:right;"> 0.1069 </td>
   <td style="text-align:right;"> 0.6045 </td>
   <td style="text-align:right;"> 0.0299 </td>
   <td style="text-align:right;"> 0.3558 </td>
   <td style="text-align:right;"> 0.0747 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 81 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5799 </td>
   <td style="text-align:right;"> 0.1087 </td>
   <td style="text-align:right;"> 0.5937 </td>
   <td style="text-align:right;"> 0.0266 </td>
   <td style="text-align:right;"> 0.3459 </td>
   <td style="text-align:right;"> 0.0746 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5427 </td>
   <td style="text-align:right;"> 0.1101 </td>
   <td style="text-align:right;"> 0.5954 </td>
   <td style="text-align:right;"> 0.0288 </td>
   <td style="text-align:right;"> 0.3246 </td>
   <td style="text-align:right;"> 0.0739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5391 </td>
   <td style="text-align:right;"> 0.1173 </td>
   <td style="text-align:right;"> 0.5899 </td>
   <td style="text-align:right;"> 0.0278 </td>
   <td style="text-align:right;"> 0.3194 </td>
   <td style="text-align:right;"> 0.0767 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 86 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.6096 </td>
   <td style="text-align:right;"> 0.0998 </td>
   <td style="text-align:right;"> 0.6078 </td>
   <td style="text-align:right;"> 0.0270 </td>
   <td style="text-align:right;"> 0.3723 </td>
   <td style="text-align:right;"> 0.0725 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 86 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.6149 </td>
   <td style="text-align:right;"> 0.0952 </td>
   <td style="text-align:right;"> 0.6040 </td>
   <td style="text-align:right;"> 0.0291 </td>
   <td style="text-align:right;"> 0.3733 </td>
   <td style="text-align:right;"> 0.0714 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 88 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.4604 </td>
   <td style="text-align:right;"> 0.1316 </td>
   <td style="text-align:right;"> 0.5828 </td>
   <td style="text-align:right;"> 0.0278 </td>
   <td style="text-align:right;"> 0.2698 </td>
   <td style="text-align:right;"> 0.0825 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 88 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.4769 </td>
   <td style="text-align:right;"> 0.1270 </td>
   <td style="text-align:right;"> 0.5771 </td>
   <td style="text-align:right;"> 0.0289 </td>
   <td style="text-align:right;"> 0.2763 </td>
   <td style="text-align:right;"> 0.0780 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 91 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5954 </td>
   <td style="text-align:right;"> 0.1230 </td>
   <td style="text-align:right;"> 0.6157 </td>
   <td style="text-align:right;"> 0.0292 </td>
   <td style="text-align:right;"> 0.3684 </td>
   <td style="text-align:right;"> 0.0857 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 91 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.6283 </td>
   <td style="text-align:right;"> 0.1137 </td>
   <td style="text-align:right;"> 0.6166 </td>
   <td style="text-align:right;"> 0.0318 </td>
   <td style="text-align:right;"> 0.3891 </td>
   <td style="text-align:right;"> 0.0812 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 94 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.6736 </td>
   <td style="text-align:right;"> 0.1034 </td>
   <td style="text-align:right;"> 0.6250 </td>
   <td style="text-align:right;"> 0.0338 </td>
   <td style="text-align:right;"> 0.4226 </td>
   <td style="text-align:right;"> 0.0769 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 94 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7200 </td>
   <td style="text-align:right;"> 0.0993 </td>
   <td style="text-align:right;"> 0.6365 </td>
   <td style="text-align:right;"> 0.0358 </td>
   <td style="text-align:right;"> 0.4601 </td>
   <td style="text-align:right;"> 0.0802 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 95 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.5970 </td>
   <td style="text-align:right;"> 0.1177 </td>
   <td style="text-align:right;"> 0.6069 </td>
   <td style="text-align:right;"> 0.0275 </td>
   <td style="text-align:right;"> 0.3633 </td>
   <td style="text-align:right;"> 0.0784 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 95 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.5952 </td>
   <td style="text-align:right;"> 0.1179 </td>
   <td style="text-align:right;"> 0.6023 </td>
   <td style="text-align:right;"> 0.0330 </td>
   <td style="text-align:right;"> 0.3598 </td>
   <td style="text-align:right;"> 0.0792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 97 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.6714 </td>
   <td style="text-align:right;"> 0.1257 </td>
   <td style="text-align:right;"> 0.6335 </td>
   <td style="text-align:right;"> 0.0365 </td>
   <td style="text-align:right;"> 0.4273 </td>
   <td style="text-align:right;"> 0.0921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 97 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7365 </td>
   <td style="text-align:right;"> 0.1001 </td>
   <td style="text-align:right;"> 0.6377 </td>
   <td style="text-align:right;"> 0.0402 </td>
   <td style="text-align:right;"> 0.4718 </td>
   <td style="text-align:right;"> 0.0829 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0.6954 </td>
   <td style="text-align:right;"> 0.0921 </td>
   <td style="text-align:right;"> 0.6304 </td>
   <td style="text-align:right;"> 0.0368 </td>
   <td style="text-align:right;"> 0.4405 </td>
   <td style="text-align:right;"> 0.0762 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0.7252 </td>
   <td style="text-align:right;"> 0.0869 </td>
   <td style="text-align:right;"> 0.6412 </td>
   <td style="text-align:right;"> 0.0390 </td>
   <td style="text-align:right;"> 0.4665 </td>
   <td style="text-align:right;"> 0.0725 </td>
  </tr>
</tbody>
</table>

## Estimaciones por dimension del IPM 
Dado que el Índice de Pobreza Multidimensional (IPM) está compuesto por diversas dimensiones, resulta fundamental analizar cada una de estas dimensiones de manera individual. Esto permite comprender la naturaleza compleja y multifacética de la pobreza, lo cual a su vez posibilita diseñar estrategias de reducción efectivas. Esta aproximación garantiza una toma de decisiones fundamentada, la distribución eficiente de recursos y un impacto más profundo en la mejora de las condiciones de vida de las personas vulnerables. En este contexto, los "hard estimates" previamente obtenidos para cada dimensión resultan esenciales para obtener las estimaciones correspondientes a cada una de ellas.

El proceso de cálculo se basa en una media ponderada y se aplica a la dimensión de **Hacinamiento**, siguiendo una lógica similar para las demás dimensiones del IPM.




```r
n_filtered <- censo_ipm$n
epred_mat_filtered <- epred_mat_hacinamiento_dummy
mrp_estimates <- epred_mat_filtered %*% n_filtered / sum(n_filtered)
datos <- data.frame(
  estimate = mean(mrp_estimates),
  estimate_se = sd(mrp_estimates)
)
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-43)Estimaciones nacional para  Hacinamiento</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> estimate_se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 0.213 </td>
   <td style="text-align:right;"> 0.0187 </td>
  </tr>
</tbody>
</table>

Con el objetivo de agilizar el proceso de calculo se define crea la función **agregado_dim_ipm** que hace los cálculos. La forma de uso es la siguiente. 


```r
source("Modelo_bayes_HxA/0funciones/agregado_dim_ipm.r")
datos_dam_haci <- agregado_dim_ipm(poststrat = censo_ipm,
           epredmat = epred_mat_hacinamiento_dummy,
           byMap = "dam")
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-45)Estimaciones por departamento para Hacinamiento</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> estimate_se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 05 </td>
   <td style="text-align:right;"> 0.1459 </td>
   <td style="text-align:right;"> 0.0399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 08 </td>
   <td style="text-align:right;"> 0.3660 </td>
   <td style="text-align:right;"> 0.0671 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 11 </td>
   <td style="text-align:right;"> 0.1204 </td>
   <td style="text-align:right;"> 0.0792 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 13 </td>
   <td style="text-align:right;"> 0.3635 </td>
   <td style="text-align:right;"> 0.0513 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15 </td>
   <td style="text-align:right;"> 0.1550 </td>
   <td style="text-align:right;"> 0.0226 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 17 </td>
   <td style="text-align:right;"> 0.1199 </td>
   <td style="text-align:right;"> 0.0300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 18 </td>
   <td style="text-align:right;"> 0.2599 </td>
   <td style="text-align:right;"> 0.0431 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 19 </td>
   <td style="text-align:right;"> 0.2437 </td>
   <td style="text-align:right;"> 0.0239 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 20 </td>
   <td style="text-align:right;"> 0.4038 </td>
   <td style="text-align:right;"> 0.0456 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 23 </td>
   <td style="text-align:right;"> 0.4052 </td>
   <td style="text-align:right;"> 0.0297 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 25 </td>
   <td style="text-align:right;"> 0.2021 </td>
   <td style="text-align:right;"> 0.0324 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 27 </td>
   <td style="text-align:right;"> 0.2264 </td>
   <td style="text-align:right;"> 0.0304 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 41 </td>
   <td style="text-align:right;"> 0.1828 </td>
   <td style="text-align:right;"> 0.0353 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 44 </td>
   <td style="text-align:right;"> 0.5007 </td>
   <td style="text-align:right;"> 0.0293 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 47 </td>
   <td style="text-align:right;"> 0.4352 </td>
   <td style="text-align:right;"> 0.0447 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 50 </td>
   <td style="text-align:right;"> 0.1698 </td>
   <td style="text-align:right;"> 0.0456 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 52 </td>
   <td style="text-align:right;"> 0.2445 </td>
   <td style="text-align:right;"> 0.0260 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 54 </td>
   <td style="text-align:right;"> 0.2869 </td>
   <td style="text-align:right;"> 0.0475 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 63 </td>
   <td style="text-align:right;"> 0.1112 </td>
   <td style="text-align:right;"> 0.0380 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 66 </td>
   <td style="text-align:right;"> 0.1094 </td>
   <td style="text-align:right;"> 0.0307 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 68 </td>
   <td style="text-align:right;"> 0.1795 </td>
   <td style="text-align:right;"> 0.0290 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 70 </td>
   <td style="text-align:right;"> 0.3869 </td>
   <td style="text-align:right;"> 0.0378 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 73 </td>
   <td style="text-align:right;"> 0.1977 </td>
   <td style="text-align:right;"> 0.0399 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 76 </td>
   <td style="text-align:right;"> 0.1183 </td>
   <td style="text-align:right;"> 0.0296 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 81 </td>
   <td style="text-align:right;"> 0.2794 </td>
   <td style="text-align:right;"> 0.1271 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:right;"> 0.2657 </td>
   <td style="text-align:right;"> 0.1240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 86 </td>
   <td style="text-align:right;"> 0.2891 </td>
   <td style="text-align:right;"> 0.1251 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 88 </td>
   <td style="text-align:right;"> 0.2337 </td>
   <td style="text-align:right;"> 0.1115 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 91 </td>
   <td style="text-align:right;"> 0.3269 </td>
   <td style="text-align:right;"> 0.1379 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 94 </td>
   <td style="text-align:right;"> 0.3595 </td>
   <td style="text-align:right;"> 0.1331 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 95 </td>
   <td style="text-align:right;"> 0.2962 </td>
   <td style="text-align:right;"> 0.1332 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 97 </td>
   <td style="text-align:right;"> 0.3603 </td>
   <td style="text-align:right;"> 0.1465 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 99 </td>
   <td style="text-align:right;"> 0.3506 </td>
   <td style="text-align:right;"> 0.1457 </td>
  </tr>
</tbody>
</table>

El resultado por municipio y para todas las dimensiones se muestra en la siguiente tabla
<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-46)Estimacion puntual por municipio y dimension</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam2 </th>
   <th style="text-align:right;"> Agua </th>
   <th style="text-align:right;"> Educacion </th>
   <th style="text-align:right;"> Empleo </th>
   <th style="text-align:right;"> Energia </th>
   <th style="text-align:right;"> Hacinamienot </th>
   <th style="text-align:right;"> Internet </th>
   <th style="text-align:right;"> Material </th>
   <th style="text-align:right;"> Saneamiento </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 05001 </td>
   <td style="text-align:right;"> 0.0111 </td>
   <td style="text-align:right;"> 0.6114 </td>
   <td style="text-align:right;"> 0.4378 </td>
   <td style="text-align:right;"> 0.2092 </td>
   <td style="text-align:right;"> 0.1492 </td>
   <td style="text-align:right;"> 0.3530 </td>
   <td style="text-align:right;"> 0.0018 </td>
   <td style="text-align:right;"> 0.0002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05002 </td>
   <td style="text-align:right;"> 0.0878 </td>
   <td style="text-align:right;"> 0.7534 </td>
   <td style="text-align:right;"> 0.6730 </td>
   <td style="text-align:right;"> 0.8434 </td>
   <td style="text-align:right;"> 0.1874 </td>
   <td style="text-align:right;"> 0.8642 </td>
   <td style="text-align:right;"> 0.0220 </td>
   <td style="text-align:right;"> 0.0006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05004 </td>
   <td style="text-align:right;"> 0.1545 </td>
   <td style="text-align:right;"> 0.6651 </td>
   <td style="text-align:right;"> 0.6529 </td>
   <td style="text-align:right;"> 0.6855 </td>
   <td style="text-align:right;"> 0.1672 </td>
   <td style="text-align:right;"> 0.7496 </td>
   <td style="text-align:right;"> 0.0348 </td>
   <td style="text-align:right;"> 0.0088 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05021 </td>
   <td style="text-align:right;"> 0.1118 </td>
   <td style="text-align:right;"> 0.6935 </td>
   <td style="text-align:right;"> 0.6364 </td>
   <td style="text-align:right;"> 0.5681 </td>
   <td style="text-align:right;"> 0.1645 </td>
   <td style="text-align:right;"> 0.6947 </td>
   <td style="text-align:right;"> 0.0257 </td>
   <td style="text-align:right;"> 0.0084 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05030 </td>
   <td style="text-align:right;"> 0.1277 </td>
   <td style="text-align:right;"> 0.6637 </td>
   <td style="text-align:right;"> 0.6509 </td>
   <td style="text-align:right;"> 0.6012 </td>
   <td style="text-align:right;"> 0.1591 </td>
   <td style="text-align:right;"> 0.6944 </td>
   <td style="text-align:right;"> 0.0252 </td>
   <td style="text-align:right;"> 0.0090 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05031 </td>
   <td style="text-align:right;"> 0.0044 </td>
   <td style="text-align:right;"> 0.7257 </td>
   <td style="text-align:right;"> 0.7231 </td>
   <td style="text-align:right;"> 0.6380 </td>
   <td style="text-align:right;"> 0.1800 </td>
   <td style="text-align:right;"> 0.8681 </td>
   <td style="text-align:right;"> 0.0208 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05034 </td>
   <td style="text-align:right;"> 0.2021 </td>
   <td style="text-align:right;"> 0.7032 </td>
   <td style="text-align:right;"> 0.7013 </td>
   <td style="text-align:right;"> 0.7456 </td>
   <td style="text-align:right;"> 0.1279 </td>
   <td style="text-align:right;"> 0.7300 </td>
   <td style="text-align:right;"> 0.0051 </td>
   <td style="text-align:right;"> 0.0148 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05036 </td>
   <td style="text-align:right;"> 0.1418 </td>
   <td style="text-align:right;"> 0.7144 </td>
   <td style="text-align:right;"> 0.6728 </td>
   <td style="text-align:right;"> 0.6601 </td>
   <td style="text-align:right;"> 0.1688 </td>
   <td style="text-align:right;"> 0.7502 </td>
   <td style="text-align:right;"> 0.0350 </td>
   <td style="text-align:right;"> 0.0110 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05038 </td>
   <td style="text-align:right;"> 0.1785 </td>
   <td style="text-align:right;"> 0.7516 </td>
   <td style="text-align:right;"> 0.7048 </td>
   <td style="text-align:right;"> 0.7464 </td>
   <td style="text-align:right;"> 0.1723 </td>
   <td style="text-align:right;"> 0.7921 </td>
   <td style="text-align:right;"> 0.0395 </td>
   <td style="text-align:right;"> 0.0125 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05040 </td>
   <td style="text-align:right;"> 0.1297 </td>
   <td style="text-align:right;"> 0.7391 </td>
   <td style="text-align:right;"> 0.6809 </td>
   <td style="text-align:right;"> 0.6273 </td>
   <td style="text-align:right;"> 0.1910 </td>
   <td style="text-align:right;"> 0.7343 </td>
   <td style="text-align:right;"> 0.0316 </td>
   <td style="text-align:right;"> 0.0084 </td>
  </tr>
</tbody>
</table>


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-47)Error de estimacion por municipio y dimension</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> dam2 </th>
   <th style="text-align:right;"> Agua_se </th>
   <th style="text-align:right;"> Educacion_se </th>
   <th style="text-align:right;"> Empleo_se </th>
   <th style="text-align:right;"> Energia_se </th>
   <th style="text-align:right;"> Hacinamienot_se </th>
   <th style="text-align:right;"> Internet_se </th>
   <th style="text-align:right;"> Material_se </th>
   <th style="text-align:right;"> Saneamiento_se </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 05001 </td>
   <td style="text-align:right;"> 0.0214 </td>
   <td style="text-align:right;"> 0.0855 </td>
   <td style="text-align:right;"> 0.1035 </td>
   <td style="text-align:right;"> 0.0858 </td>
   <td style="text-align:right;"> 0.0906 </td>
   <td style="text-align:right;"> 0.1019 </td>
   <td style="text-align:right;"> 0.0050 </td>
   <td style="text-align:right;"> 0.0015 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05002 </td>
   <td style="text-align:right;"> 0.0651 </td>
   <td style="text-align:right;"> 0.0666 </td>
   <td style="text-align:right;"> 0.0802 </td>
   <td style="text-align:right;"> 0.0579 </td>
   <td style="text-align:right;"> 0.0851 </td>
   <td style="text-align:right;"> 0.0570 </td>
   <td style="text-align:right;"> 0.0333 </td>
   <td style="text-align:right;"> 0.0044 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05004 </td>
   <td style="text-align:right;"> 0.1985 </td>
   <td style="text-align:right;"> 0.0731 </td>
   <td style="text-align:right;"> 0.1011 </td>
   <td style="text-align:right;"> 0.2171 </td>
   <td style="text-align:right;"> 0.0880 </td>
   <td style="text-align:right;"> 0.1489 </td>
   <td style="text-align:right;"> 0.0564 </td>
   <td style="text-align:right;"> 0.0296 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05021 </td>
   <td style="text-align:right;"> 0.1391 </td>
   <td style="text-align:right;"> 0.0774 </td>
   <td style="text-align:right;"> 0.1169 </td>
   <td style="text-align:right;"> 0.2357 </td>
   <td style="text-align:right;"> 0.0887 </td>
   <td style="text-align:right;"> 0.1695 </td>
   <td style="text-align:right;"> 0.0403 </td>
   <td style="text-align:right;"> 0.0305 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05030 </td>
   <td style="text-align:right;"> 0.1605 </td>
   <td style="text-align:right;"> 0.0686 </td>
   <td style="text-align:right;"> 0.1013 </td>
   <td style="text-align:right;"> 0.2299 </td>
   <td style="text-align:right;"> 0.0761 </td>
   <td style="text-align:right;"> 0.1632 </td>
   <td style="text-align:right;"> 0.0403 </td>
   <td style="text-align:right;"> 0.0318 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05031 </td>
   <td style="text-align:right;"> 0.0113 </td>
   <td style="text-align:right;"> 0.0658 </td>
   <td style="text-align:right;"> 0.0848 </td>
   <td style="text-align:right;"> 0.0817 </td>
   <td style="text-align:right;"> 0.0712 </td>
   <td style="text-align:right;"> 0.0540 </td>
   <td style="text-align:right;"> 0.0279 </td>
   <td style="text-align:right;"> 0.0009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05034 </td>
   <td style="text-align:right;"> 0.0704 </td>
   <td style="text-align:right;"> 0.0657 </td>
   <td style="text-align:right;"> 0.0782 </td>
   <td style="text-align:right;"> 0.0655 </td>
   <td style="text-align:right;"> 0.0629 </td>
   <td style="text-align:right;"> 0.0688 </td>
   <td style="text-align:right;"> 0.0134 </td>
   <td style="text-align:right;"> 0.0258 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05036 </td>
   <td style="text-align:right;"> 0.1821 </td>
   <td style="text-align:right;"> 0.0632 </td>
   <td style="text-align:right;"> 0.1005 </td>
   <td style="text-align:right;"> 0.2244 </td>
   <td style="text-align:right;"> 0.0826 </td>
   <td style="text-align:right;"> 0.1467 </td>
   <td style="text-align:right;"> 0.0459 </td>
   <td style="text-align:right;"> 0.0383 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05038 </td>
   <td style="text-align:right;"> 0.2192 </td>
   <td style="text-align:right;"> 0.0764 </td>
   <td style="text-align:right;"> 0.1244 </td>
   <td style="text-align:right;"> 0.2052 </td>
   <td style="text-align:right;"> 0.0870 </td>
   <td style="text-align:right;"> 0.1464 </td>
   <td style="text-align:right;"> 0.0598 </td>
   <td style="text-align:right;"> 0.0484 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 05040 </td>
   <td style="text-align:right;"> 0.1553 </td>
   <td style="text-align:right;"> 0.0732 </td>
   <td style="text-align:right;"> 0.0994 </td>
   <td style="text-align:right;"> 0.2243 </td>
   <td style="text-align:right;"> 0.0812 </td>
   <td style="text-align:right;"> 0.1558 </td>
   <td style="text-align:right;"> 0.0408 </td>
   <td style="text-align:right;"> 0.0278 </td>
  </tr>
</tbody>
</table>


