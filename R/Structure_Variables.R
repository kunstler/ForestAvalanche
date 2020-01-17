# Fonctions calculant le nombre de tige par placette,
# le coefficient de variation des diamètres et 
# le diamètre quadratique moyen


## mise auformat pour le calcul du nombre de tige du cycle 2
format_cycle2_essence  <- function(path_plot,data_basal_area){
  
  require(readxl)
  require(dplyr)
  
  tt  <-  grep("xls", list.files(path_plot),
               value = TRUE)
  list_essence  <- lapply(tt,
                          function( nn) {
                            read_excel(file.path("data", "IFN_ALL",
                                                 "IFNCYCLE2", nn),
                                       sheet = "Essence", skip = 5)})
  essenceC2 <- dplyr::bind_rows(list_essence)
  names(essenceC2) <- c("CODE_DEP","DÉPARTEMENT","CYCLE","ANNÉE_DE_RÉFÉRENCE",
                        "IDENTIFIANT_DU_POINT","CODE_ESSENCE","CODE_SOUS-UNITÉ",
                        "VOLUME_SUR_PIED_m³.ha^-1","NOMBRE_DE_TIGES",
                        "SURFACE_TERRIÈRE_m².ha^-1")
  
  filtered_esssenceC2 <- dplyr::filter(essenceC2, essenceC2$IDENTIFIANT_DU_POINT %in% data_basal_area$IDENTIFIANT_DU_POINT )
  
  return(filtered_esssenceC2)
}

######################################################################################################################
### nombre de tige

# cycle 4
stem_number_C4 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$w <- as.numeric(as.character(selected_trees$w))
  
  stem_nb_C4 <- selected_trees %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(stem_nb = sum(w)) %>% 
    tidyr::spread(espar, stem_nb)
    
  
  stem_nb_C4_tot <- selected_trees %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(stem_nb_tot = sum(w))
  
  stem_nb_C4$stem_nb_tot <- stem_nb_C4_tot$stem_nb_tot
  
  return(stem_nb_C4)
}


# Cycle 3
stem_number_C3 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  stem_nb_C3 <- selected_trees %>% dplyr::group_by(CPP, ESS) %>% 
    dplyr::summarise(stem_nb = sum(POND)) %>% 
    tidyr::spread(ESS, stem_nb)
  
  
  stem_nb_C3_tot <- selected_trees %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(stem_nb_tot = sum(POND))
  
  stem_nb_C3$stem_nb_tot <- stem_nb_C3_tot$stem_nb_tot
  
  return(stem_nb_C3)
}


# cycle 2
stem_number_C2 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
    
  stem_nb_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT, CODE_ESSENCE) %>% 
    dplyr::summarise(stem_nb = sum(NOMBRE_DE_TIGES)) %>% 
    tidyr::spread(CODE_ESSENCE, stem_nb)
    
    
  stem_nb_C2_tot <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(stem_nb_tot = sum(NOMBRE_DE_TIGES))
    
  stem_nb_C2$stem_nb_tot <- stem_nb_C2_tot$stem_nb_tot
    
  return(stem_nb_C2)
}



#############################################################################################
### coefficient de variation des diamètres

# C2 
var_coef_C2 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  names(selected_trees) <- c("CODE_DEP","DEPARTEMENT","CYCLE","ANNEE_DE_REFERENCE",
                                            "IDENTIFIANT_DU_POINT","NUMERO_ARBRE","CODE_ESSENCE",
                                            "CODE_SOUS-UNITE","CODE_DOM","AGE","CIRCONFERENCE_A_LA_SOUCHE_cm",
                                            "DIAMETRE_A_1.30_m_cm","HAUTEUR_TOTALE_m",
                                            "ACCROISSEMENT_RADIAL_IR5_mm","ACCROISSEMENT_RADIAL_IR10_mm",
                                            "VOLUME_HORS_REBUT_m^2","ACCROISSEMENT_DE_L'ARBRE_m^2.an^-1",
                                            "PONDERATION")
  
  selected_trees$diametre_pond <-  selected_trees$DIAMETRE_A_1.30_m_cm * 10^-2 * selected_trees$PONDERATION
  
  CV_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT, CODE_ESSENCE) %>% 
    dplyr::summarise(sigma = sd(diametre_pond), µ = mean(diametre_pond))
    CV_C2$Coef_var_diam <- CV_C2$sigma / CV_C2$µ 
    CV_C2 <-  CV_C2[,c(1,2,5)] %>% tidyr::spread(CODE_ESSENCE, Coef_var_diam )
    
  return(CV_C2)
}


# C3 
var_coef_C3 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  
  selected_trees$diametre_pond <-  selected_trees$C13/3.14 * selected_trees$POND
  
  CV_C3 <- selected_trees %>% dplyr::group_by(CPP, ESS) %>% 
    dplyr::summarise(sigma = sd(diametre_pond), µ = mean(diametre_pond))
  CV_C3$Coef_var_diam <- CV_C3$sigma / CV_C3$µ 
  CV_C3 <-  CV_C3[,c(1,2,5)] %>% tidyr::spread(ESS, Coef_var_diam )
  
  return(CV_C3)
}


# C4
var_coef_C4 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$w <- as.numeric(as.character(selected_trees$w))
  
  selected_trees$diametre_pond <-  selected_trees$c13/3.14 * selected_trees$w
  
  CV_C4 <- selected_trees %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(sigma = sd(diametre_pond), µ = mean(diametre_pond))
  CV_C4$Coef_var_diam <- CV_C4$sigma / CV_C4$µ 
  CV_C4 <-  CV_C4[,c(1,2,5)] %>% tidyr::spread(espar, Coef_var_diam )
  
  return(CV_C4)
}

#############################################################################################
### Diametre quadratique moyen

# DQ = 2/sqrt(pi) * sqrt(G/N)

root_mean_square <- function(G, N){
  
  DQ_calc <- 2/sqrt(3.14) * sqrt(G[,c(2:length(G))]/N[,c(2:length(N))])
  
  DQ_calc[,length(G)] <- G[,1]
  DQ_calc <- DQ_calc[,c(length(DQ_calc),1:(length(DQ_calc)-1))]
    
  return(DQ_calc)
}
  