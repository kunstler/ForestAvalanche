# Fonctions calculant la surface terriere ponderee des differents 
# jeux de donnees de l'IFN. (Prealablement mis au format)

###### attention ces formules nécessitent de charger les jeux de données
###### en début de plan drake

#Donnée du cycle 4
BA_C4_calcul <- function(data = treesC4){
  
  require(dplyr)
  require(tidyr)
  
  data$basal_area_w <- ((data$c13*10^-2)^2 / (4*3.14) ) * as.numeric(as.character(data$w))
  
  BA_C4 <- data %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) %>% 
    tidyr::spread(espar, spe_basal_area)
  
  BA_C4_tot <- data %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C4$basal_area_tot <- BA_C4_tot$basal_area_tot
  
  return(BA_C4)
}

# Donnée du cycle 3
BA_C3_calcul <- function(data = treesC3){
  
  require(dplyr)
  require(tidyr)
  
  data$basal_area_w <- (data$C13^2 / (4*3.14) ) * data$POND
  
  BA_C3 <- data %>% dplyr::group_by(CPP, ESS) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) %>% 
    tidyr::spread(ESS, spe_basal_area)
  
  BA_C3_tot <- data %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C3$basal_area_tot <- BA_C3_tot$basal_area_tot
  
  return(BA_C3)
}


# Données du cycle 2
BA_C2_calcul <- function(data = treesC2){
  
  require(dplyr)
  require(tidyr)
  
  names(data) <- c("CODE_DEP","DEPARTEMENT","CYCLE","ANNEE_DE_REFERENCE",
                   "IDENTIFIANT_DU_POINT","NUMERO_ARBRE","CODE_ESSENCE",
                   "CODE_SOUS-UNITE","CODE_DOM","AGE","CIRCONFERENCE_A_LA_SOUCHE_(cm)",
                   "DIAMETRE_A_1.30_m_(cm)","HAUTEUR_TOTALE_(m)",
                   "ACCROISSEMENT_RADIAL_IR5_(mm)","ACCROISSEMENT_RADIAL_IR10_(mm)",
                   "VOLUME_HORS_REBUT_(m^2)","ACCROISSEMENT_DE_L'ARBRE_(m^2/an)",
                   "PONDERATION")
  
  data$basal_area_w <- (((data[,12]*10^-2)^2)*3.14 / 4 ) * data[,18]
  
  BA_C2 <- data %>% dplyr::group_by(IDENTIFIANT_DU_POINT,CODE_ESSENCE) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) %>% 
    tidyr::spread(CODE_ESSENCE, spe_basal_area)
  
  BA_C2_tot <- data %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C2$basal_area_tot <- BA_C2_tot$basal_area_tot
  
  return(BA_C2)
}
