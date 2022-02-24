###########################################################
## Title:        Funcion Benchmark para estimaciones SAE ##
##               relacionadas al ingreso de los hograes  ##
## Returns:      Funcion benchmark                       ## 
## Author:       Felipe Molina - Andrés Gutierrez        ##        
## Institution:  CEPAL                                   ##
## Date:         2020                                    ##
###########################################################

# Estimaciones directas, cargar antes de ejecutar funcion.
# Dado que estara en una iteración, cargar fuera para que la función no cargue esta base 
# en cada iteración

# Funcion

# Se quita primer departamento y area rural por colinealidad en la matriz de calibracion
Benchmark <- function(CensoY){
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:ncol(MatrizCalibrada2)){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i]*(CensoY - medias[i])
  }
  a = calib(Xs = MatrizCalibrada2 , d = Xcenso$`(Intercept)`,total = rep(0,ncol(MatrizCalibrada2)),
            method=c("linear")) 
  return(a)
}

ncol(MatrizCalibrada)


Benchmark2 <- function(pobreza, extrema = FALSE){
  # Matriz benchmark para pobreza
  if(extrema == FALSE){
    prop = proporciones
  }else{
    prop = proporcionesEx
  }
  MatrizCalibrada2 = MatrizCalibrada
  for(i in 1:ncol(MatrizCalibrada2)){
    MatrizCalibrada2[,i] <- MatrizCalibrada[,i]*(pobreza - prop[i])
  }
  a = calib(Xs = MatrizCalibrada2 , d = Xcenso$`(Intercept)`,total = rep(0,ncol(MatrizCalibrada2)),
            method=c("linear")) 
  return(a)
}


# MatrizCalibrada2 = MatrizCalibrada
# system.time(
# for(i in 1:27){
#   MatrizCalibrada2[,i] <- MatrizCalibrada[,i]*(Xcenso$mpisos1 - proporciones[i])
# }
# )







