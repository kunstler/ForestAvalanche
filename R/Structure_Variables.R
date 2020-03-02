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


# Cycle 3
stem_number_C3 <- function(selected_trees, data_spe){
  
  require(dplyr)
  require(tidyr)
  
  data_spe <-  data_spe[,c("CODE","SPECIES")]
  data_spe$SPECIES <- stringr::str_to_title(gsub("_", " ", stringr::str_to_title(data_spe$SPECIES)))
  data_spe <- data_spe[!duplicated(data_spe)==TRUE,]
  
  stem_nb_C3 <- selected_trees %>% dplyr::group_by(CPP, ESS) %>% 
    dplyr::summarise(stem_nb = sum(POND)) 
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$CODE %in% stem_nb_C3$ESS)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]  
  
  stem_nb_C3_named <- dplyr::left_join(stem_nb_C3, filtered_spe, by = c("ESS" = "CODE"))
  stem_nb_C3_named[is.na(stem_nb_C3_named$SPECIES), "SPECIES"] <- c("inconnu")
  for(i in (1:length(stem_nb_C3_named[[1]]))){
    if(stem_nb_C3_named[i,"SPECIES"] == "inconnu"){
      stem_nb_C3_named[i,"SPECIES"] <- stem_nb_C3_named[i,"ESS"]
    }
  }
  
  stem_nb_C3_named <- stem_nb_C3_named[,c(-2)] %>% tidyr::spread(SPECIES, stem_nb)
  stem_nb_C3_named[is.na(stem_nb_C3_named)] <-  0
  
  stem_nb_C3_tot <- selected_trees %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(stem_nb_tot = sum(POND))
  
  stem_nb_C3_named$stem_nb_tot <- stem_nb_C3_tot$stem_nb_tot
  
  return(stem_nb_C3_named)
}


# cycle 2
stem_number_C2 <- function(selected_trees = trees_plotsC2, data_spe = speciesC32){
  
  require(dplyr)
  require(tidyr)
  
  for(i in (1:length(selected_trees[[1]]))){
    if(selected_trees[i,"CODE_ESSENCE"] == "02"){
      selected_trees[i,"CODE_ESSENCE"] <- c(2)
    } else if(selected_trees[i,"CODE_ESSENCE"] == "03"){
      selected_trees[i,"CODE_ESSENCE"] <- c(3)
    } else if(selected_trees[i,"CODE_ESSENCE"] == "05"){
      selected_trees[i,"CODE_ESSENCE"] <- c(5)
    } else if(selected_trees[i,"CODE_ESSENCE"] == "09"){
      selected_trees[i,"CODE_ESSENCE"] <- c(9)
    }
  }
  
  data_spe <-  data_spe[,c("CODE","SPECIES")]
  data_spe$SPECIES <- stringr::str_to_title(gsub("_", " ", stringr::str_to_title(data_spe$SPECIES)))
  data_spe <- data_spe[!duplicated(data_spe)==TRUE,]
    
  stem_nb_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT, CODE_ESSENCE) %>% 
    dplyr::summarise(stem_nb = sum(PONDERATION)) 
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$CODE %in% stem_nb_C2$CODE_ESSENCE)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]  
  
  stem_nb_C2$CODE_ESSENCE <- as.numeric(stem_nb_C2$CODE_ESSENCE)
  stem_nb_C2_named <- dplyr::left_join(stem_nb_C2, filtered_spe, by = c("CODE_ESSENCE" = "CODE"))
  stem_nb_C2_named[is.na(stem_nb_C2_named$SPECIES), "SPECIES"] <- c("inconnu")
  for(i in (1:length(stem_nb_C2_named[[1]]))){
    if(stem_nb_C2_named[i,"SPECIES"] == "inconnu"){
      stem_nb_C2_named[i,"SPECIES"] <- stem_nb_C2_named[i,"CODE_ESSENCE"]
    }
  }
  
  stem_nb_C2_named <- stem_nb_C2_named[,c(-2)] %>% tidyr::spread(SPECIES, stem_nb)
  stem_nb_C2_named[is.na(stem_nb_C2_named)] <-  0   
    
  stem_nb_C2_tot <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(stem_nb_tot = sum(PONDERATION))
    
  stem_nb_C2_named$stem_nb_tot <- stem_nb_C2_tot$stem_nb_tot
    
  return(stem_nb_C2_named)
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
  
  CV_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(sigma = sd(diametre_pond), µ = mean(diametre_pond, na.rm = TRUE))
    CV_C2$Coef_var_diam <- CV_C2$sigma / CV_C2$µ 
    CV_C2 <-  CV_C2[,c(1,4)]
    
  return(CV_C2)
}


# C3 
var_coef_C3 <- function(selected_trees){
  
  require(dplyr)
  require(tidyr)
  
  
  selected_trees$diametre_pond <-  selected_trees$C13/pi * selected_trees$POND
  
  CV_C3 <- selected_trees %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(sigma = sd(diametre_pond), µ = mean(diametre_pond, na.rm = TRUE))
  CV_C3$Coef_var_diam <- CV_C3$sigma / CV_C3$µ 
  CV_C3 <-  CV_C3[,c(1,4)] 
  
  return(CV_C3)
}


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
  
