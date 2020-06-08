# function to attribute a category of avalanche path to each plot depending on CLPA code
# zont, zonpi, lint and linpi

attrib_avalanche <- function(plot_IFN_CLPA){
  
  for (i in 1:length(plot_IFN_CLPA[[2]])) {
    
    if(plot_IFN_CLPA[i,"CODE_zont"] %in% c(1,2)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "IN"
    } else if(plot_IFN_CLPA[i,"CODE_zonpi"] %in% c(1,2)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "IN"
    } else if(plot_IFN_CLPA[i,"CODE_lint"] %in% c(2,5,8)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "IN"
    } else if(plot_IFN_CLPA[i,"CODE_linpi"] %in% c(1,4,7)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "IN"
    } else if(plot_IFN_CLPA[i,"CODE_zont"] %in% c(3,4,16)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "UNDETERMINATED"
    } else if(plot_IFN_CLPA[i,"CODE_zonpi"] %in% c(3,16)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "UNDETERMINATED"
    } else if(plot_IFN_CLPA[i,"CODE_lint"] %in% c(11)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "UNDETERMINATED"
    } else if(plot_IFN_CLPA[i,"CODE_linpi"] %in% c(10)){
      plot_IFN_CLPA[i,"avalanche_path"] <- "UNDETERMINATED"
    } else if (!is.na(plot_IFN_CLPA[i,"CODE_wzon"])) {
      plot_IFN_CLPA[i,"avalanche_path"] <- "UNDETERMINATED"
    } else {
      plot_IFN_CLPA[i,"avalanche_path"] <- "OUT"
    } 
    
  }
  
  return(plot_IFN_CLPA)
  
}


## function to select plots without Na and to compile mean_age, avalanche path category
## and climatique variables

plot_clean <- function(plot_IFN_CLPA, selected_trees, species, clim_var){
  
  require(dplyr)
  
  if(exists("IDENTIFIANT_DU_POINT", plot_IFN_CLPA)){
    
    clim_C2 <- clim_var[[1]]
    age <- mean_age(selected_trees,species)
    ava_path <- attrib_avalanche(plot_IFN_CLPA)
    
    # climatique variables
    to_exclude_C2 <- clim_C2[which(is.na(clim_C2[,"pH"])),"IDENTIFIANT_DU_POINT"]
    
    # mean age
    to_exclude_C2_age <- age[which(is.na(age[,"mean_age"])),"IDENTIFIANT_DU_POINT"]
    to_exclude_C2_age <- to_exclude_C2_age[["IDENTIFIANT_DU_POINT"]]
    
    # CLPA category
    to_exclude_C2_clpa <- ava_path[which(ava_path[,"avalanche_path"] == "UNDETERMINATED"),"IDENTIFIANT_DU_POINT"]
    
    # no trees
    to_exclude_trees <- plot_IFN_CLPA[which(!(plot_IFN_CLPA$IDENTIFIANT_DU_POINT %in% selected_trees$IDENTIFIANT_DU_POINT)),"IDENTIFIANT_DU_POINT"]
    to_exclude_trees <- to_exclude_trees[!duplicated(to_exclude_trees)==TRUE]
    
    #compilation
    idp_to_excl_C2 <- c(to_exclude_C2,to_exclude_C2_clpa,to_exclude_C2_age,to_exclude_trees)
    # elimination of potential duplicatation
    idp_to_excl_C2 <-  idp_to_excl_C2[!duplicated(idp_to_excl_C2)==TRUE]
    
    #compilation of mean age, avalanche path and climatique variables for selected plots
    plots_C2 <- clim_C2[which(!(clim_C2$IDENTIFIANT_DU_POINT %in% idp_to_excl_C2)),]
    plots_C2_age <- dplyr::left_join(plots_C2, age, by =c("IDENTIFIANT_DU_POINT"))
    plots_C2_age_ava_path <- dplyr::left_join(plots_C2_age, ava_path[,c("IDENTIFIANT_DU_POINT","avalanche_path")], by =c("IDENTIFIANT_DU_POINT"))
    
    plots_C2_age_ava_path$classe_age <- cut(plots_C2_age_ava_path$mean_age, seq(0,400,by = 20))
    
    plot_return <- plots_C2_age_ava_path
    
  } else if(exists("CPP", plot_IFN_CLPA)) { 
    
    clim_C3 <- clim_var[[2]]
    age <- mean_age(selected_trees,species)
    ava_path <- attrib_avalanche(plot_IFN_CLPA)
    
    # climatique variables
    to_exclude_C3 <- clim_C3[which(is.na(clim_C3[,"pH"])),"CPP"]
    
    # mean age
    to_exclude_C3_age <- age[which(is.na(age[,"mean_age"])),"CPP"]
    to_exclude_C3_age <- to_exclude_C3_age[["CPP"]]
    
    # CLPA category
    to_exclude_C3_clpa <- ava_path[which(ava_path[,"avalanche_path"] == "UNDETERMINATED"),"CPP"]
    
    # no trees
    to_exclude_trees <- plot_IFN_CLPA[which(!(plot_IFN_CLPA$CPP %in% selected_trees$CPP)),"CPP"]
    to_exclude_trees <- to_exclude_trees[!duplicated(to_exclude_trees)==TRUE]
    
    #compilation
    idp_to_excl_C3 <- c(to_exclude_C3,to_exclude_C3_clpa,to_exclude_C3_age,to_exclude_trees)
    # elimination of potential duplicatation
    idp_to_excl_C3 <-  idp_to_excl_C3[!duplicated(idp_to_excl_C3)==TRUE]
    
    #compilation of mean age, avalanche path and climatique variables for selected plots
    plots_C3 <- clim_C3[which(!(clim_C3$CPP %in% idp_to_excl_C3)),]
    plots_C3_age <- dplyr::left_join(plots_C3, age, by =c("CPP" = "CPP"))
    plots_C3_age_ava_path <- dplyr::left_join(plots_C3_age, ava_path[,c("CPP","avalanche_path")], by =c("CPP"))
    
    plots_C3_age_ava_path$classe_age <- cut(plots_C3_age_ava_path$mean_age, seq(0,400,by = 20))
    
    plot_return <- plots_C3_age_ava_path
    
  } else if(exists("idp", plot_IFN_CLPA)) {
    
    clim_C4 <- clim_var[[3]]
    age <- mean_age(selected_trees,species)
    ava_path <- attrib_avalanche(plot_IFN_CLPA)
    
    # climatique variables
    to_exclude_C4 <- clim_C4[which(is.na(clim_C4[,"pH"])),"idp"]
    
    # mean age
    to_exclude_C4_age <- age[which(is.na(age[,"mean_age"])),"idp"]
    to_exclude_C4_age <- as.numeric(as.character(to_exclude_C4_age[["idp"]]))
    
    # CLPA category
    to_exclude_C4_clpa <- ava_path[which(ava_path[,"avalanche_path"] == "UNDETERMINATED"),"idp"]
    to_exclude_C4_clpa <- as.numeric(as.character(to_exclude_C4_clpa))
    
    # no woody species
    to_exclude_trees <- plot_IFN_CLPA[which(!(plot_IFN_CLPA$idp %in% selected_trees$idp)),"idp"]
    to_exclude_trees <- as.numeric(as.character(to_exclude_trees[!duplicated(to_exclude_trees)==TRUE]))
    
    #compilation
    idp_to_excl_C4 <- c(to_exclude_C4,to_exclude_C4_clpa,to_exclude_C4_age,to_exclude_trees)
    # elimination of potentiel duplicatation
    idp_to_excl_C4 <-  idp_to_excl_C4[!duplicated(idp_to_excl_C4)==TRUE]
    
    #compilation of mean age, avalanche path and climatique variables for selected plots
    plots_C4 <- clim_C4[which(!(clim_C4$idp %in% idp_to_excl_C4)),]
    plots_C4_age <- dplyr::left_join(plots_C4, age, by =c("idp" = "idp"))
    plots_C4_age_ava_path <- dplyr::left_join(plots_C4_age, ava_path[,c("idp","avalanche_path")], by =c("idp"))
    
    plots_C4_age_ava_path$classe_age <- cut(plots_C4_age_ava_path$mean_age, seq(0,580,by = 20))
    
    plotsC4_NA_traits <- subset(plots_C4_age_ava_path,plots_C4_age_ava_path$idp != c("433625"))
    plotsC4_selected <- subset(plotsC4_NA_traits,plotsC4_NA_traits$idp != c("723623"))
    
    plot_return <- plotsC4_selected
    
  }
  
  return(plot_return)
  
}


