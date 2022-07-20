modelo <- function(y_si, setdata) {
  ######################## IMPORTANTE ######################################
  ##########################################################################
  ## Creada específicamente para los modelos empleado en los departamento. #
  ##########################################################################
  setdata$y_si <- setdata[[y_si]]
  setdata$y_no <- setdata$n - setdata[[y_si]]
  cat("Modelo para ", y_si, "\n")
  glmer(
    cbind(y_si, y_no) ~ (1 | mpio) +
      edad +
      area +
      ipm_Material+
      ipm_Hacinamiento+
      ipm_Agua+
      ipm_Saneamiento +
      ipm_Energia + 
      ipm_Internet +
      sexo  + tasa_desocupacion +
      F182013_stable_lights + 
      X2016_crops.coverfraction +
      X2016_urban.coverfraction  ,
    family = binomial(link = "logit"),
    data = setdata,
  )
}




# Calculo del ipm global
aux_ipm <- function(data_MC_boot) {
  iter_ipm_boot <-  pmap(as.list(data_MC_boot),
                         function(mpio,
                                  n,
                                  ipm_Material,
                                  ipm_Saneamiento,
                                  ipm_Energia,
                                  ipm_Internet,
                                  ipm_Agua,
                                  ipm_Hacinamiento,
                                  prob_boot_educacion,
                                  prob_boot_empleo) {
                           y_empleo =  rbinom(n, 1, prob = prob_boot_empleo)
                           y_educacion =  rbinom(n, 1, prob = prob_boot_educacion)
                           ipm <- 0.1 * (
                             ipm_Material +
                               ipm_Saneamiento +
                               ipm_Energia +
                               ipm_Internet +
                               ipm_Agua +
                               ipm_Hacinamiento
                           ) +
                             0.2 * (y_educacion +   y_empleo)
                           
                           ipm_dummy <- ifelse(ipm < 0.4, 0, 1)
                           mean(ipm_dummy)
                         })
  
  data_MC_boot$ipm <- unlist(iter_ipm_boot)
  
  data_MC_boot %>% group_by(mpio) %>%
    summarise(ipm = sum((n * ipm)) / sum(n))
}
