# fonctions to compute plot mean age of dominants trees



# function creating table to correct C4 ages
correctif_age <- function(data_spe, selected_trees){
  
  filtered_spe <- dplyr::filter(data_spe, data_spe$ESPAR %in% selected_trees$espar)
  filtered_spe <-  filtered_spe[!duplicated(filtered_spe),]
  
  trois_ans <- c("10")
  quatre_ans <- c("51","64","12V")
  cinq_ans <- c("17C")
  six_ans <- c("06")
  sept_ans <- c("02","05","11","52")
  huit_ans <- c("03","54","62")
  neuf_ans <- c("09","61")
  autres_resineux <- c("58","59","63","67","53CO","72V")
  
  for(i in 1:length(filtered_spe[[1]])){
    if(filtered_spe[i,"ESPAR"] %in% trois_ans){
      filtered_spe[i,"correctif_age"] <- 3 
    } else if(filtered_spe[i,"ESPAR"] %in% quatre_ans){
      filtered_spe[i,"correctif_age"] <- 4
    } else if(filtered_spe[i,"ESPAR"] %in% cinq_ans){
      filtered_spe[i,"correctif_age"] <- 5
    } else if(filtered_spe[i,"ESPAR"] %in% six_ans){
      filtered_spe[i,"correctif_age"] <- 6
    } else if(filtered_spe[i,"ESPAR"] %in% sept_ans){
      filtered_spe[i,"correctif_age"] <- 7
    } else if(filtered_spe[i,"ESPAR"] %in% huit_ans){
      filtered_spe[i,"correctif_age"] <- 8
    } else if(filtered_spe[i,"ESPAR"] %in% neuf_ans){
      filtered_spe[i,"correctif_age"] <- 9
    } else if(filtered_spe[i,"ESPAR"] %in% autres_resineux){
      filtered_spe[i,"correctif_age"] <- 6
    } else {
      filtered_spe[i,"correctif_age"] <- 5 # autres feuillus
    }
  }  
  return(filtered_spe)
}  


# CYCLE 2 :
mean_age_C2 <- function(selected_trees){
  
  require(dplyr)
  
  Age_C2 <- selected_trees %>% dplyr::group_by(IDENTIFIANT_DU_POINT) %>% 
    dplyr::summarise(mean_age = mean(AGE, na.rm = TRUE))
  
  return(Age_C2)
}


# Cycle 3
mean_age_C3 <- function(selected_trees){
  
  require(dplyr)
  
  Age_C3 <- selected_trees %>% dplyr::group_by(CPP) %>% 
    dplyr::summarise(mean_age = mean(AGE, na.rm = TRUE))
  
  return(Age_C3)
}


# Cycle 4
mean_age_C4 <- function(selected_trees, data_spe){
  
  require(dplyr)
  
  df_correctif <- correctif_age(data_spe, selected_trees)
  
  
  selected_trees$idp <- as.numeric(as.character(selected_trees$idp))
  selected_trees$age <- as.numeric(as.character(selected_trees$age))
  selected_trees_cor <- right_join(selected_trees, df_correctif[,c("ESPAR","correctif_age")], by = c("espar" = "ESPAR"))
  selected_trees_cor$age_30cm <- selected_trees_cor$age + selected_trees_cor$correctif_age
  
  Age_C4 <- selected_trees_cor %>% dplyr::group_by(idp) %>% 
    dplyr::summarise(mean_age = mean(age_30cm, na.rm = TRUE))
  return(Age_C4)
}


# general function calling each other

mean_age <- function(selected_trees, species){
  
  if(exists("idp", selected_trees)){
    age <- mean_age_C4(selected_trees, species)
  } else if(exists("CPP", selected_trees)){
    age <- mean_age_C3(selected_trees)
  } else if(exists("IDENTIFIANT_DU_POINT", selected_trees)){
    age <- mean_age_C2(selected_trees)
  }
  
  return(age)
  
}












