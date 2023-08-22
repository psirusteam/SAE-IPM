estime_IPM <-
  function(poststrat,
           chain_q,
           byMap = c("dam", "etnia", "sexo"), 
           n_dim = 8) {

    ## Creación de variable para el calculo nacional
    if(is.null(byMap)){
      poststrat  %<>% mutate(Nacional = "Nacional")
      byMap <- "Nacional"
    }
    
    chain_Ind <- chain_q
    
    chain_Ind[chain_Ind <= 0.4] <- 0
    chain_Ind[chain_Ind != 0] <- 1
    
    ## Creación de indicadora poscición
    poststrat2 <- poststrat %>% ungroup() %>%
      mutate(Posi = 1:n()) %>%
      group_by_at(byMap) %>% group_nest()
    
    ## Creación de alertas por eliminar las categosrías de anoest
    
    if(any(byMap == "anoest")){
      poststrat2 %<>% filter(!anoest %in% c("99", "98"))
      cat("
     ############################# NOTA #################################
     # En las tabla de escolaridad (anoest) se eliminan los conteos de  #
     # NA y NS/NR                                                       #
     ############################# NOTA #################################
      ")
    }
    ## Estimado los mrp
    Estimado_mrp <- poststrat2 %>%
      mutate(Estimado_mrp =
               map(data,
                   function(subgrupo) {
                     filtering_condition <- subgrupo$Posi
                     n_filtered <- subgrupo$n
                     chain_Ind_sub <- chain_Indicadora[,filtering_condition]
                     chain_q_sub <- chain_q[,filtering_condition]
                     
                     if(nrow(subgrupo)==1){
                       D <- (chain_Ind_sub*n_filtered)
                       Q <- (chain_q_sub*n_filtered  ) 
                     }else{
                     D <- rowSums(chain_Ind_sub*n_filtered)
                     Q <- rowSums(chain_q_sub*n_filtered)
                     }
                     chain_H =  D/sum(n_filtered)
                     chain_A = Q/(sum(n_filtered)*n_dim)
                  
                     chain_IPM <- (D*Q)/((sum(n_filtered)^2)*n_dim)
                    
                     data.frame(
                       H = mean(chain_H),
                       H_se = sd(chain_H),
                       A = mean(chain_A),
                       A_se = sd(chain_A),
                       ipm = mean(chain_IPM),
                       ipm_se = sd(chain_IPM)
                     )
                   }), data = NULL) %>% unnest("Estimado_mrp")
 
    return(Estimado_mrp)
  }
