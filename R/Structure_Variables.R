# Fonctions calculant le nombre de tige par placette,
# le coefficient de variation des diamètres et 
# le diamètre quadratique moyen


######################################################################################################################
### nombre de tige

# cycle 4
stem_number_C4 <- function(selected_trees, data_spe){
  
  require(dplyr)
  require(tidyr)
  require(stringr)
  
  selected_trees$w <- as.numeric(as.character(selected_trees$w))
  
  data_spe <-  data_spe[,c("ESPAR","Latin_Name")]
  data_spe$Latin_Name <- stringr::str_to_title(gsub("_", " ", stringr::str_to_title(data_spe$Latin_Name)))
  
  stem_nb_C4 <- selected_trees %>% dplyr::group_by(idp, espar) %>% 
    dplyr::summarise(stem_nb = sum(w))
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$ESPAR %in% stem_nb_C4$espar)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]  
  
  stem_nb_C4_named <- dplyr::left_join(stem_nb_C4, filtered_spe, by = c("espar" = "ESPAR"))
  stem_nb_C4_named[is.na(stem_nb_C4_named$Latin_Name), "Latin_Name"] <- c("inconnu")
  for(i in (1:length(stem_nb_C4_named[[1]]))){
    if(stem_nb_C4_named[i,"Latin_Name"] == "inconnu"){
      stem_nb_C4_named[i,"Latin_Name"] <- stem_nb_C4_named[i,"espar"]
    }
  }
     
  stem_nb_C4_named <- stem_nb_C4_named[,c(-2)] %>% tidyr::spread(Latin_Name, stem_nb)
  stem_nb_C4_named[is.na(stem_nb_C4_named)] <-  0 
  
  stem_nb_C4_tot <- selected_trees %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(stem_nb_tot = sum(w))
  
  stem_nb_C4_named$stem_nb_tot <- stem_nb_C4_tot$stem_nb_tot
  
  return(stem_nb_C4_named)
}





#############################################################################################
### coefficient de variation des diamètres


# C4
var_coef_C4 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  selected_trees$w <- as.numeric(as.character(selected_trees$w))
  
  selected_trees$diametre_pond <-  selected_trees$c13/pi * selected_trees$w
  
  CV_C4 <- selected_trees %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(sigma = sd(diametre_pond), µ = mean(diametre_pond, na.rm = TRUE))
  CV_C4$Coef_var_diam <- CV_C4$sigma / CV_C4$µ 
  CV_C4 <-  CV_C4[,c(1,4)]
  
  return(CV_C4)
}

#############################################################################################
### Diametre quadratique moyen

# DQ = 2/sqrt(pi) * sqrt(G/N)

root_mean_square <- function(G, N){
  
  DQ_calc <- 2/sqrt(pi) * sqrt(G[,length(G)]/N[,length(N)])
  
  DQ_calc[,2] <- G[,1]
  DQ_calc <- DQ_calc[,c(2,1)]
  names(DQ_calc) <- c("idp","RMS_diam")
  
  return(DQ_calc)
}
  
