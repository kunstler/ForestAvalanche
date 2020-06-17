# Fonctions calculant la surface terriere ponderee des differents 
# jeux de donnees de l'IFN. (Prealablement mis au format)

#Donnée du cycle 4
BA_C4_calcul <- function(selected_trees, data_spe){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$basal_area_w <- ((selected_trees$c13*10^-2)^2 / (4*pi) ) * as.numeric(as.character(selected_trees$w))
  
  BA_C4 <- selected_trees %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(spe_basal_area = sum(basal_area_w))
  
  data_spe <-  data_spe[,c("ESPAR","Latin_Name")]
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$ESPAR %in% BA_C4$espar)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
  
  
  BA_C4_named <- dplyr::left_join(BA_C4, filtered_spe, by = c("espar" = "ESPAR"))
  BA_C4_named[is.na(BA_C4_named$Latin_Name), "Latin_Name"] <- c("inconnu")
  for(i in (1:length(BA_C4_named[[1]]))){
    if(BA_C4_named[i,"Latin_Name"] == "inconnu"){
      BA_C4_named[i,"Latin_Name"] <- BA_C4_named[i,"espar"]
    }
  }
  BA_C4_named <-  tidyr::spread(BA_C4_named[,c("idp","Latin_Name","spe_basal_area")], 
                                Latin_Name, spe_basal_area)
  
  BA_C4_tot <- selected_trees %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(basal_area_tot = sum(basal_area_w))
  
  BA_C4_named$basal_area_tot <- BA_C4_tot$basal_area_tot
  BA_C4_named[is.na(BA_C4_named)] <-  0
  
  return(BA_C4_named)
}

