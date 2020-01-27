# Fonctions calculant la surface terriere ponderee des differents 
# jeux de donnees de l'IFN. (Prealablement mis au format)

#Donnée du cycle 4
BA_C4_calcul <- function(selected_trees, data_spe){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$basal_area_w <- ((selected_trees$c13*10^-2)^2 / (4*pi) ) * as.numeric(as.character(selected_trees$w))
  
  BA_C4 <- selected_trees %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w))
  
  data_spe <-  data_spe[,c("code","Latin_name")]
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$code %in% BA_C4$espar)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
  
  
  BA_C4_named <- dplyr::left_join(BA_C4, filtered_spe, by = c("espar" = "code"))
  BA_C4_named[is.na(BA_C4_named$Latin_name), "Latin_name"] <- c("inconnu")
  for(i in (1:length(BA_C4_named[[1]]))){
    if(BA_C4_named[i,"Latin_name"] == "inconnu"){
      BA_C4_named[i,"Latin_name"] <- BA_C4_named[i,"espar"]
    }
  }
  BA_C4_named <-  tidyr::spread(BA_C4_named[,c("idp","Latin_name","spe_basal_area")], 
                                Latin_name, spe_basal_area)
  
  BA_C4_tot <- selected_trees %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C4_named$basal_area_tot <- BA_C4_tot$basal_area_tot
  BA_C4_named[is.na(BA_C4_named)] <-  0
  
  return(BA_C4_named)
}

# Donnée du cycle 3
BA_C3_calcul <- function(selected_trees, data_spe){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$basal_area_w <- (selected_trees$C13^2 / (4*pi) ) * selected_trees$POND
  
  BA_C3 <- selected_trees %>% dplyr::group_by(CPP, ESS) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w)) 
  
  data_spe <-  data_spe[,c("CODE","SPECIES")]
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$CODE %in% BA_C3$ESS)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
  
  
  BA_C3_named <- dplyr::left_join(BA_C3, filtered_spe, by = c("ESS" = "CODE"))
  BA_C3_named[is.na(BA_C3_named$SPECIES), "SPECIES"] <- c("inconnu")
  for(i in (1:length(BA_C3_named[[1]]))){
    if(BA_C3_named[i,"SPECIES"] == "inconnu"){
      BA_C3_named[i,"SPECIES"] <- BA_C3_named[i,"ESS"]
    }
  }
  BA_C3_named <-  tidyr::spread(BA_C3_named[,c("CPP","SPECIES","spe_basal_area")], 
                                SPECIES, spe_basal_area)
  
  BA_C3_tot <- selected_trees %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C3_named$basal_area_tot <- BA_C3_tot$basal_area_tot
  BA_C3_named[is.na(BA_C3_named)] <-  0
  
  return(BA_C3_named)
}


# Données du cycle 2
BA_C2_calcul <- function(selected_trees, data_spe){
  
  require(dplyr)
  require(tidyr)
  
  
  selected_trees$basal_area_w <- (((selected_trees[,12]*10^-2)^2)*pi / 4 ) * selected_trees[,18]
  
  BA_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT,CODE_ESSENCE) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w))
  
  data_spe <-  data_spe[,c("CODE","SPECIES")]
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$CODE %in% BA_C2$CODE_ESSENCE)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
  
  
  BA_C2_named <- dplyr::left_join(BA_C2, filtered_spe, by = c("CODE_ESSENCE" = "CODE"))
  BA_C2_named[is.na(BA_C2_named$SPECIES), "SPECIES"] <- c("inconnu")
  for(i in (1:length(BA_C2_named[[1]]))){
    if(BA_C2_named[i,"SPECIES"] == "inconnu"){
      BA_C2_named[i,"SPECIES"] <- BA_C2_named[i,"CODE_ESSENCE"]
    }
  }
  BA_C2_named <-  tidyr::spread(BA_C2_named[,c("IDENTIFIANT_DU_POINT","SPECIES","spe_basal_area")], 
                                SPECIES, spe_basal_area)
  
  BA_C2_tot <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C2_named$basal_area_tot <- BA_C2_tot$basal_area_tot
  BA_C2_named[is.na(BA_C2_named)] <-  0
  
  return(BA_C2_named)
}
