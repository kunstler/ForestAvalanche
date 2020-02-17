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
  
  require(plyr)
  
  if(exists("IDENTIFIANT_DU_POINT", plot_IFN_CLPA)){
    
    clim_C2 <- clim_var[[1]]
    age <- mean_age(selected_trees,species)
    ava_path <- attrib_avalanche(plot_IFN_CLPA)
    
    # climatique variables
    to_exclude_C2 <- clim_C2[which(is.na(clim_C2[,c("pH","cation_exchange_capacity","organic_carbon_content")])),"IDENTIFIANT_DU_POINT"]
    
    # mean age
    to_exclude_C2_age <- age[which(is.na(age[,"mean_age"])),"IDENTIFIANT_DU_POINT"]
    to_exclude_C2_age <- to_exclude_C2_age[["IDENTIFIANT_DU_POINT"]]
    
    # CLPA category
    to_exclude_C2_clpa <- ava_path[which(ava_path[,"avalanche_path"] == "UNDETERMINATED"),"IDENTIFIANT_DU_POINT"]
    
    #compilation
    idp_to_excl_C2 <- c(to_exclude_C2,to_exclude_C2_clpa,to_exclude_C2_age)
    # elimination of potential duplicatation
    idp_to_excl_C2 <-  idp_to_excl_C2[!duplicated(idp_to_excl_C2)==TRUE]
    
    #compilation of mean age, avalanche path and climatique variables for selected plots
    plots_C2 <- clim_C2[which(!(clim_C2$IDENTIFIANT_DU_POINT %in% idp_to_excl_C2)),]
    plots_C2_age <- join(plots_C2, age, by =c("IDENTIFIANT_DU_POINT") , type = "left", match = "all")
    plots_C2_age_ava_path <- join(plots_C2_age, ava_path[,c("IDENTIFIANT_DU_POINT","avalanche_path")], by =c("IDENTIFIANT_DU_POINT") , type = "left", match = "all")
    
    plot_return <- plots_C2_age_ava_path
    
  } else if(exists("CPP", plot_IFN_CLPA)) { 
    
    clim_C3 <- clim_var[[2]]
    age <- mean_age(selected_trees,species)
    ava_path <- attrib_avalanche(plot_IFN_CLPA)
    
    # climatique variables
    to_exclude_C3 <- clim_C3[which(is.na(clim_C3[,c("pH","cation_exchange_capacity","organic_carbon_content")])),"CPP"]
    
    # mean age
    to_exclude_C3_age <- age[which(is.na(age[,"mean_age"])),"CPP"]
    to_exclude_C3_age <- to_exclude_C3_age[["CPP"]]
    
    # CLPA category
    to_exclude_C3_clpa <- ava_path[which(ava_path[,"avalanche_path"] == "UNDETERMINATED"),"CPP"]
    
    #compilation
    idp_to_excl_C3 <- c(to_exclude_C3,to_exclude_C3_clpa,to_exclude_C3_age)
    # elimination of potential duplicatation
    idp_to_excl_C3 <-  idp_to_excl_C3[!duplicated(idp_to_excl_C3)==TRUE]
    
    #compilation of mean age, avalanche path and climatique variables for selected plots
    plots_C3 <- clim_C3[which(!(clim_C3$CPP %in% idp_to_excl_C3)),]
    plots_C3_age <- join(plots_C3, age, by =c("CPP") , type = "left", match = "all")
    plots_C3_age_ava_path <- join(plots_C3_age, ava_path[,c("CPP","avalanche_path")], by =c("CPP") , type = "left", match = "all")
    
    plot_return <- plots_C3_age_ava_path
    
  } else if(exists("idp", plot_IFN_CLPA)) {
    
    clim_C4 <- clim_var[[3]]
    age <- mean_age(selected_trees,species)
    ava_path <- attrib_avalanche(plot_IFN_CLPA)
    
    # climatique variables
    to_exclude_C4 <- clim_C4[which(is.na(clim_C4[,c("pH","cation_exchange_capacity","organic_carbon_content")])),"idp"]
    
    # mean age
    to_exclude_C4_age <- age[which(is.na(age[,"mean_age"])),"idp"]
    to_exclude_C4_age <- as.numeric(as.character(to_exclude_C4_age[["idp"]]))
    
    # CLPA category
    to_exclude_C4_clpa <- ava_path[which(ava_path[,"avalanche_path"] == "UNDETERMINATED"),"idp"]
    to_exclude_C4_clpa <- as.numeric(as.character(to_exclude_C4_clpa))
    
    #compilation
    idp_to_excl_C4 <- c(to_exclude_C4,to_exclude_C4_clpa,to_exclude_C4_age)
    # elimination of potential duplicatation
    idp_to_excl_C4 <-  idp_to_excl_C4[!duplicated(idp_to_excl_C4)==TRUE]
    
    #compilation of mean age, avalanche path and climatique variables for selected plots
    plots_C4 <- clim_C4[which(!(clim_C4$idp %in% idp_to_excl_C4)),]
    plots_C4_age <- join(plots_C4, age, by =c("idp") , type = "left", match = "all")
    plots_C4_age_ava_path <- join(plots_C4_age, ava_path[,c("idp","avalanche_path")], by =c("idp") , type = "left", match = "all")
    
    plot_return <- plots_C4_age_ava_path
    
  }
  
  return(plot_return)
  
}





