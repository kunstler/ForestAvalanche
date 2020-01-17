# Fonctions calculant la surface terriere ponderee des differents 
# jeux de donnees de l'IFN. (Prealablement mis au format)

###### attention ces formules nécessitent de charger les jeux de données
###### en début de plan drake

#Donnée du cycle 4
BA_C4_calcul <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$basal_area_w <- ((selected_trees$c13*10^-2)^2 / (4*3.14) ) * as.numeric(as.character(selected_trees$w))
  
  BA_C4 <- selected_trees %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) %>% 
    tidyr::spread(espar, spe_basal_area)
  
  BA_C4_tot <- selected_trees %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C4$basal_area_tot <- BA_C4_tot$basal_area_tot
  
  return(BA_C4)
}

# Donnée du cycle 3
BA_C3_calcul <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$basal_area_w <- (selected_trees$C13^2 / (4*3.14) ) * selected_trees$POND
  
  BA_C3 <- selected_trees %>% dplyr::group_by(CPP, ESS) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) %>% 
    tidyr::spread(ESS, spe_basal_area)
  
  BA_C3_tot <- selected_trees %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C3$basal_area_tot <- BA_C3_tot$basal_area_tot
  
  return(BA_C3)
}


# Données du cycle 2
BA_C2_calcul <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  
  selected_trees$basal_area_w <- (((selected_trees[,12]*10^-2)^2)*3.14 / 4 ) * selected_trees[,18]
  
  BA_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT,CODE_ESSENCE) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) %>% 
    tidyr::spread(CODE_ESSENCE, spe_basal_area)
  
  BA_C2_tot <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C2$basal_area_tot <- BA_C2_tot$basal_area_tot
  
  return(BA_C2)
}
