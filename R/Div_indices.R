
# function to cumputes functional diversity indices

##loadd(traitsC4, basal_area_C4, height ,seed_mass)

functional_ind <- function(traits, basal_area , height, seed_mass){
  
  require(stringr)
  require(FD)
  require(dplyr)
  
  
  if(exists("IDENTIFIANT_DU_POINT", basal_area)){
    
    height$SPECIES <- str_to_title(gsub("_"," ",seed_mass$SPECIES)) 
    height_grp <- height %>% dplyr::group_by(SPECIES) %>% 
      dplyr::summarise(mean_height_genre = mean(mean_height, na.rm = TRUE))
    
    seed_mass$SPECIES <- str_to_title(gsub("_"," ",height$SPECIES)) 
    seed_mass_grp <- seed_mass %>% dplyr::group_by(SPECIES) %>% 
      dplyr::summarise(seed_mass_meangrp = mean(seed_mass_mean, na.rm = TRUE))
    
    traits$SPECIES <- str_to_title(traits$SPECIES)
    traits <- left_join(traits, height_grp[,c("SPECIES","mean_height_genre")], by = c("SPECIES" = "SPECIES"))
    traits <- left_join(traits, seed_mass_grp[,c("SPECIES","seed_mass_meangrp")], by = c("SPECIES" = "SPECIES"))
    names(basal_area) <- str_to_title(gsub("_"," ",names(basal_area)))
    traits_C2 <- traits[which(traits$SPECIES %in% colnames(basal_area)),c(2,3,4,5,9,11,13,15,17,19,21,23,25,27,31,33,35,36)]
    
    row.names(traits_C2) <- traits_C2[,"SPECIES"]
    traits_C2 <- traits_C2[,c(-1)]
    traits_C2 <- traits_C2[ order(row.names(traits_C2)),]
    
    basal_area_C2 <- basal_area[,c(-1,-length(basal_area))]
    basal_area_C2 <- basal_area_C2[ order(names(basal_area_C2))]
    NAMES <- basal_area[["Identifiant Du Point"]]
    row.names(basal_area_C2) <- NAMES
    
    FD <- dbFD(traits_C2,basal_area_C2, corr = "lingoes")
    
  }  else  if(exists("CPP", basal_area)){
    
    height$SPECIES <- str_to_title(gsub("_"," ",height$SPECIES)) 
    height_grp <- height %>% dplyr::group_by(SPECIES) %>% 
      dplyr::summarise(mean_height_genre = mean(mean_height, na.rm = TRUE))
    
    seed_mass$SPECIES <- str_to_title(gsub("_"," ",seed_mass$SPECIES)) 
    seed_mass_grp <- seed_mass %>% dplyr::group_by(SPECIES) %>% 
      dplyr::summarise(seed_mass_meangrp = mean(seed_mass_mean, na.rm = TRUE))
    
    traits$SPECIES <- str_to_title(traits$SPECIES)
    traits <- left_join(traits, height_grp[,c("SPECIES","mean_height_genre")], by = c("SPECIES" = "SPECIES"))
    traits <- left_join(traits, seed_mass_grp[,c("SPECIES","seed_mass_meangrp")], by = c("SPECIES" = "SPECIES"))
    names(basal_area) <- str_to_title(gsub("_"," ",names(basal_area)))
    traits_C3 <- traits[which(traits$SPECIES %in% colnames(basal_area)),c(2,3,4,5,9,11,13,15,17,19,21,23,25,27,31,33,35,36)]
    
    row.names(traits_C3) <- traits_C3[,"SPECIES"]
    traits_C3 <- traits_C3[,c(-1)]
    traits_C3 <- traits_C3[ order(row.names(traits_C3)),]
    
    basal_area_C3 <- basal_area[,c(-1,-length(basal_area))]
    basal_area_C3 <- basal_area_C3[ order(names(basal_area_C3))]
    NAMES <- basal_area[["CPP"]]
    row.names(basal_area_C3) <- NAMES
    
    
    FD <- dbFD(traits_C3,basal_area_C3, corr = "lingoes", calc.FRic = FALSE, calc.FGR = FALSE, calc.FDiv = FALSE)
    
  } else if(exists("idp", basal_area)) {
    
    colonneC4 <- c(2,3,4,5,9,11,13,15,17,19,21,23,25,27,31,33,35,36,37,38,39,40) -1
    
    height$Latin_Name <- str_to_title(gsub("_"," ",height$Latin_Name)) 
    height_grp <- height %>% dplyr::group_by(Latin_Name) %>% 
      dplyr::summarise(mean_height = mean(mean_height, na.rm = TRUE))
    height_grp_log <- height %>% dplyr::group_by(Latin_Name) %>% 
      dplyr::summarise(log_max_height = log(mean(mean_height, na.rm = TRUE)))
    
    seed_mass$Latin_Name <- str_to_title(gsub("_"," ",seed_mass$Latin_Name)) 
    seed_mass_grp <- seed_mass %>% dplyr::group_by(Latin_Name) %>% 
      dplyr::summarise(mean_seed_mass = mean(seed_mass_mean, na.rm = TRUE))
    seed_mass_grp_log <- seed_mass %>% dplyr::group_by(Latin_Name) %>% 
      dplyr::summarise(log_seed_mass = log(mean(seed_mass_mean, na.rm = TRUE)))
    
    traits$sp <- str_to_title(traits$sp)
    traits$log_SLA <- log(traits$SLA)
    traits$log_Wood_density <- log(traits$Wood_density)
    traits <- left_join(traits, height_grp[,c("Latin_Name","mean_height")], by = c("sp" = "Latin_Name"))
    traits <- left_join(traits, seed_mass_grp[,c("Latin_Name","mean_seed_mass")], by = c("sp" = "Latin_Name"))
    traits <- left_join(traits, height_grp_log[,c("Latin_Name","log_max_height")], by = c("sp" = "Latin_Name"))
    traits <- left_join(traits, seed_mass_grp_log[,c("Latin_Name","log_seed_mass")], by = c("sp" = "Latin_Name"))
    names(basal_area) <- str_to_title(gsub("_"," ",names(basal_area)))
    traits_C4 <- traits[which(traits$sp %in% colnames(basal_area)),colonneC4]
    
    row.names(traits_C4) <- traits_C4[,"sp"]
    traits_C4 <- traits_C4[,c(-1)]
    traits_C4 <- traits_C4[ order(row.names(traits_C4)),]
    
    basal_area_C4 <- basal_area[,c(-1,-length(basal_area))]
    basal_area_C4 <- basal_area_C4[ order(names(basal_area_C4))]
    NAMES <- as.numeric(as.character(basal_area[["Idp"]]))
    row.names(basal_area_C4) <- NAMES
    
    FD <- dbFD(traits_C4,basal_area_C4, corr = "lingoes")
    
  }
  
  return(FD)
  
}


taxo_ind <- function(stem_nb){
  
  require(stringr)
  require(FD)
  require(dplyr)
  require(hillR)
  
  
  
  if(exists("IDENTIFIANT_DU_POINT", stem_nb)){
     
    stemnb_C2 <- stem_nb[,c(-1,-length(stem_nb))]
    stemnb_C2 <- stemnb_C2[ order(names(stemnb_C2))]
    NAMES <- stem_nb[["IDENTIFIANT_DU_POINT"]]
    row.names(stemnb_C2) <- NAMES
    
    q0 <- hill_taxa(stemnb_C2, q = 0)
    q1 <- as.numeric(as.character(hill_taxa(stemnb_C2, q = 1)))
    q2 <- hill_taxa(stemnb_C2, q = 2)
    
  }  else  if(exists("CPP", stem_nb)){
    
    stemnb_C3 <- stem_nb[,c(-1,-length(stem_nb))]
    stemnb_C3 <- stemnb_C3[ order(names(stemnb_C3))]
    NAMES <- stem_nb[["CPP"]]
    row.names(stemnb_C3) <- NAMES
    
    q0 <- hill_taxa(stemnb_C3, q = 0)
    q1 <- hill_taxa(stemnb_C3, q = 1)
    q2 <- hill_taxa(stemnb_C3, q = 2)
    
  } else if(exists("idp", stem_nb)) {
    
    stemnb_C4 <- stem_nb[,c(-1,-length(stem_nb))]
    stemnb_C4 <- stemnb_C4[ order(names(stemnb_C4))]
    NAMES <- stem_nb[["idp"]]
    row.names(stemnb_C4) <- NAMES
    
    q0 <- hill_taxa(stemnb_C4, q = 0)
    q1 <- hill_taxa(stemnb_C4, q = 1)
    q2 <- hill_taxa(stemnb_C4, q = 2)
  }
  
  return(list(q0,q1,q2))
}

