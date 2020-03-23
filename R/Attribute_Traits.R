# function to attribute trait value to each species of a plot

traits_value <- function(selected_trees = treesCLPA_C2, data_spe = speciesC32, traits = traitsC32){
  
  require(dplyr)
  require(tidyr)
  require(stringr)
  
  clear_name <- c("idp","Code_esp","Latin_Name", "SLA","Narea","Nmass", "SLA_GenusM",
                  "Narea_GenusM", "Nmass_GenusM", "Leaf_size_cm2", "Leaf_size_cm2_GenusM",
                  "Whole_leaf_size_cm2", "Whole_leaf_size_cm2_GenusM", "Wood_density",
                  "Wood_density_GenusM","A_mm_2","A_mm_2_GenusM", "F_mm_2_mm_2",
                  "F_mm_2_mm_2_GenusM","N_mm_2","N_mm_2_GenusM", "S_mm_4","S_mm_4_GenusM",
                  "PI50","PI50_GenusM", "PI88","PI88_GenusM", "Psi_min_midday",
                  "Psi_min_midday_GenusM", "Psi_min","Psi_min_GenusM","Psi_50_safety_margin",
                  "Psi_50_safety_margin_GenusM", "Psi_88_safety_margin","Psi_88_safety_margin_GenusM")  
  
  if(exists("idp", selected_trees)){
    
    selected_trees <- selected_trees[,c("idp","espar")]
    plot_spe <- selected_trees %>% dplyr::ungroup() %>% dplyr::group_by(espar)
    plot_spe <- plot_spe[!duplicated(plot_spe)==TRUE,]
    
    
    data_spe$Latin_Name <- gsub("_", " ", stringr::str_to_title(data_spe$Latin_Name))
    data_spe <-  data_spe[,c("ESPAR","Latin_Name")]
    
    
    filtered_spe <- dplyr::filter(data_spe, data_spe$ESPAR %in% plot_spe$espar)
    filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
    
    plot_spe_named <- dplyr::left_join(plot_spe, filtered_spe, by = c("espar" = "ESPAR"))
    plot_spe_named[is.na(plot_spe_named$Latin_Name), "Latin_Name"] <- c("inconnu")
    for(i in (1:length(plot_spe_named[[1]]))){
      if(plot_spe_named[i,"Latin_Name"] == "inconnu"){
        plot_spe_named[i,"Latin_Name"] <- plot_spe_named[i,"espar"]
      }
    }
    
    plot_spe_trait <- left_join(plot_spe_named, traits, by = c("Latin_Name" = "sp"))
    plot_spe_trait$Latin_Name <- str_to_title(plot_spe_trait$Latin_Name)
    
  } else if(exists("CPP", selected_trees)){
    
    selected_trees <- selected_trees[,c("CPP","ESS")]
    plot_spe <- selected_trees %>% dplyr::ungroup() %>% dplyr::group_by(ESS)
    plot_spe <- plot_spe[!duplicated(plot_spe)==TRUE,]
    
    
    data_spe$Latin_Name <- gsub("_", " ", str_to_title(data_spe$Latin_Name))
    data_spe$SPECIES <- gsub("_", " ", str_to_title(data_spe$SPECIES))
    data_spe <-  data_spe[,c("CODE","Latin_Name","SPECIES")]
    
    
    filtered_spe <- dplyr::filter(data_spe, data_spe$CODE %in% plot_spe$ESS)
    filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
    
    plot_spe_named <- dplyr::left_join(plot_spe, filtered_spe, by = c("ESS" = "CODE"))
    plot_spe_named[is.na(plot_spe_named$Latin_Name), "Latin_Name"] <- c("inconnu")
    for(i in (1:length(plot_spe_named[[1]]))){
      if(plot_spe_named[i,"Latin_Name"] == "inconnu"){
        plot_spe_named[i,"Latin_Name"] <- plot_spe_named[i,"ESS"]
      }
    }
    traits$SPECIES <- str_to_title(traits$SPECIES)
    plot_spe_trait <- left_join(plot_spe_named[,c("CPP","ESS")], traits , by = c("ESS" = "CODE"))
    plot_spe_trait <-  plot_spe_trait[!duplicated(plot_spe_trait)==TRUE,]
    
  } else if(exists("IDENTIFIANT_DU_POINT", selected_trees)){
    
    selected_trees <- selected_trees[,c("IDENTIFIANT_DU_POINT","CODE_ESSENCE")]
    plot_spe <- selected_trees %>% dplyr::ungroup() %>% dplyr::group_by(CODE_ESSENCE)
    plot_spe <- plot_spe[!duplicated(plot_spe)==TRUE,]
    
    
    data_spe$Latin_Name <- gsub("_", " ", str_to_title(data_spe$Latin_Name))
    data_spe$SPECIES <- gsub("_", " ", str_to_title(data_spe$SPECIES))
    data_spe <-  data_spe[,c("CODE","Latin_Name","SPECIES")]
    
    
    filtered_spe <- dplyr::filter(data_spe, data_spe$CODE %in% plot_spe$CODE_ESSENCE)
    filtered_spe <-  filtered_spe[!duplicated(filtered_spe)==TRUE,]
    
    plot_spe_named <- dplyr::left_join(plot_spe, filtered_spe, by = c("CODE_ESSENCE" = "CODE"))
    plot_spe_named[is.na(plot_spe_named$Latin_Name), "Latin_Name"] <- c("inconnu")
    for(i in (1:length(plot_spe_named[[1]]))){
      if(plot_spe_named[i,"Latin_Name"] == "inconnu"){
        plot_spe_named[i,"Latin_Name"] <- plot_spe_named[i,"CODE_ESSENCE"]
      }
    }
    traits$SPECIES <- str_to_title(traits$SPECIES)
    plot_spe_trait <- left_join(plot_spe_named[,c("IDENTIFIANT_DU_POINT","CODE_ESSENCE")],
                                traits , by = c("CODE_ESSENCE" = "CODE"))
    plot_spe_trait <-  plot_spe_trait[!duplicated(plot_spe_trait)==TRUE,]
  }
  
  names(plot_spe_trait) <- clear_name
  return(plot_spe_trait)
}


Attrib_heigth <- function(speciesC32, speciesC4){
  
  library(drake)
  library(stringr)
  library(dplyr)
  
  speciesC32$Latin_Name <- str_to_title(speciesC32$Latin_Name)
  speciesC4$Latin_Name <- str_to_title(speciesC4$Latin_Name)
  
  speciesC32_complet <- left_join(speciesC32,speciesC4[,c("ESPAR","Latin_Name")], 
                                  by = c("Latin_Name" = "Latin_Name"))
  
  height <- read.csv(file.path("data", "max_height.csv"), header = TRUE, sep = ",")
  height$sp <- gsub("sp.","",height$sp)
  
  Height_C4 <- left_join(speciesC4, height, by = c("ESPAR" = "sp"))
  colnames(Height_C4) <- c("code","French_name", "Latin_Name", "mean_height","sd_mean_height", "nb_observations")
  Height_C32 <- left_join(speciesC32_complet,height, by = c("ESPAR" = "sp"))
  Height_C32 <- Height_C32[,c(-4,-5)]
  colnames(Height_C32) <- c("code","SPECIES", "Latin_Name", "mean_height","sd_mean_height", "nb_observations")
  
  return(list(Height_C32,Height_C4))
}


Attrib_seed_mass <- function(speciesC32, speciesC4){
  
  library(drake)
  library(stringr)
  library(dplyr)
  
  speciesC32$Latin_Name <- str_to_title(speciesC32$Latin_Name)
  speciesC4$Latin_Name <- str_to_title(speciesC4$Latin_Name)
  
  speciesC32_complet <- left_join(speciesC32,speciesC4[,c("ESPAR","Latin_Name")], 
                                  by = c("Latin_Name" = "Latin_Name"))
  
  seed_mass <- read.csv(file.path("data","Traits", "traits.csv"), header = TRUE, sep = ",")
  seed_mass$sp <- gsub("sp.","",seed_mass$sp)
  seed_mass$Latin_name <- str_to_title(seed_mass$Latin_name)
  seed_mass <- seed_mass[,c(1,2,4,12)]
  
  
  seed_mass_C4 <- left_join(speciesC4, seed_mass, by = c("ESPAR" = "sp"))
  seed_mass_C4 <- seed_mass_C4[,c(-4)]
  colnames(seed_mass_C4) <- c("code","French_name", "Latin_Name", "seed_mass_mean","genus")
  
  seed_mass_C32 <- left_join(speciesC32_complet,seed_mass, by = c("ESPAR" = "sp"))
  seed_mass_C32 <- seed_mass_C32[,c("CODE","SPECIES", "Latin_Name", "Seed.mass.mean","Seed.mass.genus")]
  colnames(seed_mass_C32) <- c("code","SPECIES", "Latin_Name", "seed_mass_mean","genus")
  
  return(list(seed_mass_C32,seed_mass_C4))
}