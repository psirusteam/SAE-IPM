


# Índice de Pobreza Multidimensional

El Índice de Pobreza Multidimensional (IPM) es una medida que captura la pobreza desde múltiples dimensiones. Se calcula utilizando ponderaciones y umbrales en función de diferentes variables o indicadores que reflejan aspectos diversos de la calidad de vida.Los componentes del IPM de describen a continuación:  

1.    Headcount Ratio (H)

   Este componente mide la proporción de personas que están privadas en al menos una de las dimensiones consideradas. Matemáticamente, $H$ se calcula como la proporción entre el número de personas privadas y la población total:
$$
      H = \frac{1}{N} \sum_{i=1}^{N} I\left( q_{i} > z \right)
$$
con 
$$
      q_i =  \sum_{k=1}^{K} w_k \cdot y_{i}^{k}  
$$

Donde:

  -   $N$ es el número de individuos u hogares en la población. 
  -   $K$ es el número de dimensiones o indicadores de la privación.
  -   $w_k$ es el ponderador asociado a la dimensión $k$.
  -   $y_{i}^{k}$ es una variable binaria que toma el valor $1$ si el individuo $i$ esta privado  en la dimensión $k$ y $0$ en el caso contrario. 
  -   $z$ es el umbral para considerar a alguien con multiples privaciones.
  
 

2.    Intensity of Deprivation (A) 

Este componente mide la intensidad o gravedad promedio de la privación entre aquellos que están privados. Matemáticamente, $A$ se calcula como el promedio de los indicadoras $y_{i}^{k}$ para aquellos hogares o personas que están privados:   
  
$$ 
A = \frac{1}{NK} \sum_{i=1}^{N} \sum_{k=1}^{K} q_i = \frac{1}{N} \sum_{i=1}^{N} \bar{q_i}  
$$

Luego, el Índice de Pobreza Multidimensional (IPM) se expresa como:

$$
IPM = H \times A
$$
reemplazando las $H$ y $A$ por sus respectivas ecuaciones se tiene que: 

$$
IPM =\left[ \frac{1}{N} \sum_{i=1}^{N} I\left( q_{i} > z \right) \right] \left[  \frac{1}{NK} \sum_{i=1}^{N} \sum_{k=1}^{K} q_i  \right]
$$
de donde se sigue 

$$
IPM = \frac{1}{N^2K} \left[\sum_{i=1}^{N} I\left( q_{i} > z \right) \right] \left[ \sum_{i=1}^{N} \sum_{k=1}^{K} q_i  \right]
$$


El IPM combina los componentes $H$ y $A$. 

## Ejemplo  
Para ilustrar el cálculo del Índice de Pobreza Multidimensional (IPM), empleamos el conjunto de datos simulados a continuación.

Consideremos un escenario con diez observaciones ($N = 10$), a las cuales se les han asignado ocho dimensiones distintas ($K = 8$). Estas dimensiones están ponderadas de acuerdo con el vector $w = (0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2)$.


```r
# Definición de parámetros
K <- 8  # Número de dimensiones
N <- 10 # Número de personas
w <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2)  # Ponderaciones para cada dimensión
```

A continuación se realiza la simulación de la matriz de variables dicótomicas para las diez persona respondientes 


```r
set.seed(1234)
# Generación de datos simulados
M_dummy <- matrix(sample(x = c(1,0),size =  N * K, replace = TRUE),
                   nrow = N, ncol = K)  # Matriz de dimensiones aleatorias

colnames(M_dummy)<- paste0("y",1:K)
rownames(M_dummy)<- paste0("P",1:N)
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

A continuación se realiza el calculo de H y A usando las ecuaciones dadas previamente. 

```r
# Cálculo de H y A utilizando el vector q
q <- M_dummy %*% w  # Vector q de ponderaciones por dimensiones
Indicadora <- ifelse(q > 0.4,1,0)
datos <- data.frame(M_dummy, q = q, Indicadora) 
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
   <th style="text-align:right;"> q </th>
   <th style="text-align:right;"> Indicadora </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.8 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

Para obtener la primera parte de la ecuación hacemos la suma de la indicadora, es decir:  

```r
D <- sum(Indicadora)
D
```

```
## [1] 4
```
El segundo elemento del IPM se obtiene al sumar los $q_i$ 


```r
Q <- sum(q)
Q
```

```
## [1] 4.2
```
Luego, el IPM se obtiene como: 


```r
IPM <- (D * Q)/((N^2)*K)  # Cálculo del IPM
IPM
```

```
## [1] 0.021
```

#  Estimación del modelo de unidad para variables Binarias

En muchas aplicaciones, la variable de interés en áreas pequeñas puede ser binaria, por ejemplo, $y_{di} = 0$ ó $1$, representando la ausencia (o no) de una característica específica. En el caso binario, la estimación objetivo en cada dominio $d = 1, \dotso, D$ puede ser la proporción $\bar{Y}_d = \pi_d = \frac{1}{N_d} \sum_{i=1}^{N_d} y_{di}$ de la población que tiene esta característica, donde $N=\sum_{d=1}^{D}N_d$ y   $\pi_{di}$ es la probabilidad de que una unidad específica $i$ en el dominio $d$ obtenga el valor 1.

Aunque se han propuesto otros métodos para resultados binarios, como el modelado basado en M-cuantiles (Chambers et al., 2016), en esta aplicación seguimos el enfoque tradicional basado en modelos mixtos lineales generalizados. En este escenario, $\pi_{di}$ se modela con una función de enlace logit definida como:

$$
\text{logit}(\pi_{di}) = \log \left( \frac{\pi_{di}}{1 - \pi_{di}} \right) = \eta_{di} = \mathbf{x}_{di}^\top \mathbf{\beta} + u_d 
$$


con $i = 1, \ldots, N_d$, $d = 1, \ldots, D$, $\boldsymbol{\beta}$ un vector de parámetros de efectos fijos y $u_d$ el efecto aleatorio específico del área para el dominio $d$ con $u_d \sim N(0, \sigma_u^2)$. Se asume que $u_d$ son independientes y $y_{di} | u_d \sim \text{Bernoulli}(\pi_{di})$ con $E(y_{di} | u_d) = \pi_{di}$ y $\text{Var}(y_{di} | u_d) = \sigma^2_{di} = \pi_{di} (1 - \pi_{di})$. Además, $\mathbf{x}_{di}$ representa el vector $p \times 1$ de valores de las variables auxiliares a nivel de unidad.

Dado que nuestro problema involucra la identificación de privaciones en forma de valores binarios $(0,1)$ en relación con varios indicadores, hemos optado por utilizar un modelo mixto logit Bernoulli a nivel de unidad como punto de partida. Hay varios algoritmos para ajustar este tipo de modelo, incluyendo el método de momentos simulados (MSM), el algoritmo de expectación-maximización (EM), el algoritmo de verosimilitud cuasi-penalizada (PQL) y el algoritmo de aproximación de máxima verosimilitud Laplace (ML-Laplace). 

Los métodos bayesianos también se pueden utilizar para ajustar modelos mixtos logit Bernoulli. Uno de los métodos más comunes es el algoritmo de Markov Chain Monte Carlo (MCMC). Este algoritmo genera muestras de los parámetros del modelo a partir de su distribución posterior, que es la distribución de los parámetros dada la evidencia.

Otro método bayesiano para ajustar modelos mixtos logit Bernoulli es el enfoque de máxima verosimilitud aproximada (MAP). Este enfoque se basa en la idea de encontrar los parámetros del modelo que maximizan la verosimilitud de los datos, suponiendo que los parámetros siguen una distribución a priori.

Los métodos bayesianos tienen varias ventajas sobre los métodos clásicos para ajustar modelos mixtos logit Bernoulli. En primer lugar, los métodos bayesianos pueden incorporar información previa sobre los parámetros del modelo, lo que puede mejorar la precisión de las estimaciones. En segundo lugar, los métodos bayesianos pueden proporcionar intervalos de credivilidad para los parámetros del modelo, que pueden ser utilizados para hacer inferencias sobre el modelo. En tercer lugar, los métodos bayesianos pueden ser utilizados para generar predicciones del modelo, que pueden ser utilizadas para tomar decisiones.

Sin embargo, los métodos bayesianos también tienen algunas desventajas. Por ejemplo, los métodos bayesianos pueden ser más sensibles a la elección de la distribución a previas, lo que puede dificultar la obtención de estimaciones precisas. Para evitar, eso en nuestro ejercicio se hará uso de distribuciones previas no informativas, es decir, podemos definir distribuciones previas 

$$
\begin{eqnarray*}
\beta_m & \sim   & N(0, 10000)\\
\sigma^2_u &\sim & IG(0.0001,0.0001)
\end{eqnarray*}
$$
El modelo se debe estimar para cada una de las dimensiones. 
  


## Estimación de $\pi^{k}_{di}$

La estimación de $\pi^{k}_{di}$ refleja la probabilidad de que una unidad específica $i$ en el dominio $d$ obtenga el valor 1 en la dimensión $k$. Para llevar a cabo esta estimación, seguimos el siguiente procedimiento:

$$
\bar{Y}^{k}_d = \pi^{k}_d = \frac{1}{N_d} \sum_{i=1}^{N_d} y^{k}_{di}
$$

Aquí, $y^{k}_{di}$ puede tomar los valores 0 ó 1, representando la ausencia (o no) de una característica específica. Ahora, podemos dividir la suma en dos partes: $s_d$, que representa las unidades observadas en una muestra, y $s_d^c$, que son las unidades no observados. Por lo tanto,

$$
\begin{equation*}
\bar{Y}^{k}_d = \pi^{k}_d =  \frac{1}{N_d}\left(\sum_{s_d}y^{k}_{di} + \sum_{s^c_d}y^{k}_{di} \right) 
\end{equation*}
$$
Ahora, suponga que mediante un modelo de unidad es posible realizar la predicción de  $y^{k}_{di}$ para las unidades no observadas. De esta manera, el estimador de $\pi^{k}_d$ se expresa como:

$$
\hat{\pi}^{k}_d = \frac{1}{N_d}\left( \sum_{s_d}y^{k}_{di} + \sum_{s^c_d}\hat{y}^{k}_{di} \right)
$$

Donde,

$$\hat{y}^{k}_{di}=E_{\mathscr{M}}\left(y^{k}_{di}\mid\boldsymbol{x}_{d},\boldsymbol{\beta}\right)$$

Aquí, $\mathscr{M}$ hace referencia a la medida de probabilidad inducida por el modelo. Sin embargo, en la práctica, individualizar a las unidades observadas  y no observadas en una encuesta de hogares puede ser difícil. Por lo tanto, una alternativa es realizar la predicción $\hat{y}^{k}_{di}$ para todas las observaciones en el universo. De esta manera, la estimación $\hat{\pi}^{k}_d$ se simplifica a:

$$
\hat{\pi}^{k}_d = \frac{1}{N_d}\sum_{i=1}^{N_d}\hat{y}^{k}_{di}
$$

Este enfoque permite estimar la probabilidad $\pi^{k}_d$ en el dominio $d$ en la dimensión $k$ utilizando predicciones y datos disponibles en lugar de contar con información individual detallada para todos los casos.

## Pedicción de los Hard estimates  

Hobza y Morales (2016) definen los "hard estimates" como valores binarios (0 o 1) que indican de manera precisa si un individuo tiene o no una característica específica en relación con cada indicador de pobreza multidimensional. Estas estimaciones reflejan la naturaleza binaria de la información, facilitando el cálculo de indicadores y tasas de incidencia de pobreza. Estas estimaciones desempeñan un papel clave en la determinación de la incidencia de la pobreza multidimensional, ya que indican la presencia o ausencia de privaciones en indicadores específicos para cada individuo. Esto plantea un desafío en la estimación, ya que no se trata solo de obtener valores finales, sino de precisar si las características están presentes o no en indicadores faltantes. Con la definición de los *hard estimates*, y sabiendo que la estimación de $\pi^{k}_{di}$ refleja la probabilidad de que una unidad específica $i$ en el dominio $d$ obtenga el valor 1 en la dimensión $k$ se define $\hat{y}^{k}_{di} \sim Bernoulli(\hat{\pi}^{k}_{di})$


## Estimación Puntual del Índice de Pobreza Multidimensional (IPM)

Supongamos que el Índice de Pobreza Multidimensional está compuesto por $K$ dimensiones o indicadores para cada individuo $i$ en el censo. El procedimiento propuesto para estimar el IPM es el siguiente:

  1. Utilice los datos de la muestra para ajustar un modelo logit Bernoulli a nivel de unidad para cada indicador. Esto se logra mediante el uso del algoritmo de Markov Chain Monte Carlo (MCMC) con $L$ iteraciones.

  2. Para cada dimensión $k$ a la cual se le para ajustó un modelo logit Bernoulli a nivel de unidad con $L$ iteraciones, realice la predicción de los valores $\hat{y}^{k}_{di}$ para cada individuo en el censo. Esto generará $L$ realizaciones aleatorias de $\hat{y}^{k}_{di}$.

  3. Denotemos como $\hat{y}_{di}^{kl}$ a la $l$-ésima realización aleatoria de la dimensión $k$ para el individuo $i$ en el dominio $d$. Calculamos $q_{di}^{l} = \sum_{k=1}^{K} w_k \cdot y_{di}^{kl}$. Luego, podemos calcular $H_d^{l}$,  $A_d^{l}$ y $IPM_{d}^{l}$ utilizando las ecuaciones:

$$
      H_d^{l} = \frac{1}{N_d} \sum_{i=1}^{N_d} I\left( q_{di}^{l} > z \right),
$$

$$ 
A_d^{l} = \frac{1}{N_dK} \sum_{i=1}^{N_d} \sum_{k=1}^{K} q_{di}^{l}
$$

y 

$$
IPM_{d}^{l} = H_{d}^{l} \times A_{d}^{l}
$$

   4. La estimación puntual de $H_d$,  $A_{d}$ y $IPM_{d}$ en cada área pequeña $d$ se calcula tomando el promedio sobre cada una de las $L$ iteraciones: 
    
$$
    \hat{H}_d = \frac{1}{L}\sum_{l=1}^{L}H_d^l, 
$$
    
$$
        \hat{A}_d = \frac{1}{L}\sum_{l=1}^{L}A_d^l 
$$
  y  
$$
        \widehat{IPM}_d = \frac{1}{L}\sum_{l=1}^{L}IPM_d^l 
$$
    
  5. Dada que el modelo se estimó usando el algoritmo MCMC, es posible tener la estimación del error de estimación, de esta forma: 
  
$$
  \widehat{Var}(\hat{H}_d) = \frac{1}{L}\sum_{l=1}^{L}\left( H^{l}_{d} -\hat{H}_d \right)^2,
$$
  
$$
  \widehat{Var}(\hat{A}_d) = \frac{1}{L}\sum_{l=1}^{L}\left( A^{l}_{d} -\hat{A}_d \right)^2
$$ 
y

$$
  \widehat{Var}(\widehat{IPM}_d) = \frac{1}{L}\sum_{l=1}^{L}\left( IPM_d^{l} -\widehat{IPM}_d \right)^2
$$


## Ejemplo: Aplicación de la metodología. 

Para ilustrar el cálculo del Índice de Pobreza Multidimensional (IPM), empleando el algoritmo MCMC empleamos el conjunto de datos simulados a continuación.

Consideremos un escenario con diez unidades ($N = 10$), a las cuales se les han asignado ocho dimensiones distintas ($K = 8$). Estas dimensiones están ponderadas de acuerdo con el vector $w = (0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2)$.


```r
# Definición de parámetros
K <- 8  # Número de dimensiones
N <- 10 # Número de personas
w <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.2, 0.2)  # Ponderaciones para cada dimensión
```

A continuación se realiza la simulación de la matriz de variables dicótomicas para las diez unidades, para ello asumiremos que $\hat{\pi}^{k}_{di} =  (0.5, 0.2, 0.3, 0.4, 0.1, 0.9, 0.2, 0.7)$ que fueron estimados mediante modelos mixtos logit Bernoulli, Bayesiano con $L = 5$. Con las que se obtienes las siguientes matrices de Hard estimates


```r
set.seed(1234)
library(purrr)
pi_di = c(0.5, 0.2, 0.3, 0.4, 0.1, 0.9, 0.2, 0.7)
# Generación de datos simulados

## Iteración 1 
data_1 <- data.frame(
  y1 = as.numeric(rbernoulli(n = N, p= pi_di[1])),
  y2 = as.numeric(rbernoulli(n = N, p= pi_di[2])),
  y3 = as.numeric(rbernoulli(n = N, p= pi_di[3])),
  y4 = as.numeric(rbernoulli(n = N, p= pi_di[4])),
  y5 = as.numeric(rbernoulli(n = N, p= pi_di[5])),
  y6 = as.numeric(rbernoulli(n = N, p= pi_di[6])),
  y7 = as.numeric(rbernoulli(n = N, p= pi_di[7])),
  y8 = as.numeric(rbernoulli(n = N, p= pi_di[8]))
)
rownames(data_1)<- paste0("P",1:N)
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-11)Iteración (l=1)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-12)Iteración (l=2)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-13)Iteración (l=3)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-14)Iteración (l=4)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-15)Iteración (l=5)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> y1 </th>
   <th style="text-align:right;"> y2 </th>
   <th style="text-align:right;"> y3 </th>
   <th style="text-align:right;"> y4 </th>
   <th style="text-align:right;"> y5 </th>
   <th style="text-align:right;"> y6 </th>
   <th style="text-align:right;"> y7 </th>
   <th style="text-align:right;"> y8 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>


A continuación se realiza el calculo de $q^l_i$ y $I\left( q_{i}^{l} > z \right)$ usando las ecuaciones dadas previamente y $z=0.4$. 


```r
# Vector q de ponderaciones por dimensiones
q1 <- as.matrix(data_1) %*% w  
q2 <- as.matrix(data_2) %*% w  
q3 <- as.matrix(data_3) %*% w  
q4 <- as.matrix(data_4) %*% w  
q5 <- as.matrix(data_5) %*% w  

Indicadora1 <- ifelse(q1 > 0.4,1,0)
Indicadora2 <- ifelse(q2 > 0.4,1,0)
Indicadora3 <- ifelse(q3 > 0.4,1,0)
Indicadora4 <- ifelse(q4 > 0.4,1,0)
Indicadora5 <- ifelse(q5 > 0.4,1,0)

datos <- data.frame(q1,q2,q3,q4,q5, 
                    Indicadora1,Indicadora2,Indicadora3, Indicadora4,Indicadora5) 
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-17)Iteración (l=1)</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> q1 </th>
   <th style="text-align:right;"> q2 </th>
   <th style="text-align:right;"> q3 </th>
   <th style="text-align:right;"> q4 </th>
   <th style="text-align:right;"> q5 </th>
   <th style="text-align:right;"> Indicadora1 </th>
   <th style="text-align:right;"> Indicadora2 </th>
   <th style="text-align:right;"> Indicadora3 </th>
   <th style="text-align:right;"> Indicadora4 </th>
   <th style="text-align:right;"> Indicadora5 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> P1 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P2 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P3 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P4 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P5 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P6 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.1 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P7 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P8 </td>
   <td style="text-align:right;"> 0.2 </td>
   <td style="text-align:right;"> 0.3 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P9 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.4 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> P10 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.6 </td>
   <td style="text-align:right;"> 0.5 </td>
   <td style="text-align:right;"> 0.7 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
</tbody>
</table>

Ahora se calcula $H^l$, $A^l$ y $IPM^l$, esto es:  


```r
H_l <- colSums(datos[,6:10])/N
H_l
```

```
## Indicadora1 Indicadora2 Indicadora3 Indicadora4 Indicadora5 
##         0.4         0.6         0.5         0.4         0.5
```


```r
A_l <- colSums(datos[,1:5])/(N*K)
A_l
```

```
##      q1      q2      q3      q4      q5 
## 0.04750 0.05125 0.05500 0.05375 0.05750
```


```r
IPM_l <- as.numeric(H_l*A_l)
IPM_l
```

```
## [1] 0.01900 0.03075 0.02750 0.02150 0.02875
```

Por último se realiza el calculo de las estimaciones puntuales y su varianza para $H$, $A$ y $IPM$, esto es:  


```r
estimacion <- data.frame(H = mean(H_l),
           H_sd = sd(H_l),
           A = mean(A_l),
           A_sd = sd(A_l),
           IPM = mean(IPM_l),
           IPM_sd = sd(IPM_l))
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
<caption>(\#tab:unnamed-chunk-22)Estimaciones</caption>
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
   <td style="text-align:right;"> 0.48 </td>
   <td style="text-align:right;"> 0.0837 </td>
   <td style="text-align:right;"> 0.053 </td>
   <td style="text-align:right;"> 0.0038 </td>
   <td style="text-align:right;"> 0.0255 </td>
   <td style="text-align:right;"> 0.005 </td>
  </tr>
</tbody>
</table>
