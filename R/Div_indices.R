

# function to cumputes functional diversity indices

functional_ind <- function(traits,stem_nb){
  
  require(stringr)
  require(FD)
  require(dplyr)
  
  
  if(exists("IDENTIFIANT_DU_POINT", stem_nb)){

  traits$SPECIES <- str_to_title(traits$SPECIES)
  traits_C2 <- traits[which(traits$SPECIES %in% colnames(stem_nb)),c(2,3,4,5,9,11,13,15,17,19,21,23,25,27,31,33)]

  row.names(traits_C2) <- traits_C2[,"SPECIES"]
  traits_C2 <- traits_C2[,c(-1)]
  traits_C2 <- traits_C2[ order(row.names(traits_C2)),]

  stemnb_C2 <- stem_nb[,c(-1,-37)]
  stemnb_C2 <- stemnb_C2[ order(names(stemnb_C2))]
  NAMES <- stem_nb[["IDENTIFIANT_DU_POINT"]]
  row.names(stemnb_C2) <- NAMES

  FD <- dbFD(traits_C2,stemnb_C2, corr = "lingoes")

  }  else  if(exists("CPP", stem_nb)){
    
  traits$SPECIES <- str_to_title(traits$SPECIES)
  traits_C3 <- traits[which(traits$SPECIES %in% colnames(stem_nb)),c(2,3,4,5,9,11,13,15,17,19,21,23,25,27,31,33)]

  row.names(traits_C3) <- traits_C3[,"SPECIES"]
  traits_C3 <- traits_C3[,c(-1)]
  traits_C3 <- traits_C3[ order(row.names(traits_C3)),]

  stemnb_C3 <- stem_nb[,c(-1,-32)]
  stemnb_C3 <- stemnb_C3[ order(names(stemnb_C3))]
  NAMES <- stem_nb[["CPP"]]
  row.names(stemnb_C3) <- NAMES

  FD <- dbFD(traits_C3,stemnb_C3, corr = "lingoes")

  } else if(exists("idp", stem_nb)) {

  colonneC4 <- c(2,3,4,5,9,11,13,15,17,19,21,23,25,27,31,33) -1

  traits$sp <- str_to_title(traits$sp)
  traits_C4 <- traits[which(traits$sp %in% colnames(stem_nb)),colonneC4]

  row.names(traits_C4) <- traits_C4[,"sp"]
  traits_C4 <- traits_C4[,c(-1)]
  traits_C4 <- traits_C4[ order(row.names(traits_C4)),]

  stemnb_C4 <- stem_nb[,c(-1,-57)]
  stemnb_C4 <- stemnb_C4[ order(names(stemnb_C4))]
  NAMES <- stem_nb[["idp"]]
  row.names(stemnb_C4) <- NAMES

  FD <- dbFD(traits_C4,stemnb_C4, corr = "lingoes")
  
  }

  return(FD)
  
}



taxo_ind <- function(stem_nb){
  
  require(stringr)
  require(FD)
  require(dplyr)
  require(hillR)
  
  
  
  if(exists("IDENTIFIANT_DU_POINT", stem_nb)){
     
    stemnb_C2 <- stem_nb[,c(-1,-37)]
    stemnb_C2 <- stemnb_C2[ order(names(stemnb_C2))]
    NAMES <- stem_nb[["IDENTIFIANT_DU_POINT"]]
    row.names(stemnb_C2) <- NAMES
    
    q0 <- hill_taxa(stemnb_C2, q = 0)
    q1 <- as.numeric(as.character(hill_taxa(stemnb_C2, q = 1)))
    q2 <- hill_taxa(stemnb_C2, q = 2)
    
  }  else  if(exists("CPP", stem_nb)){
    
    stemnb_C3 <- stem_nb[,c(-1,-32)]
    stemnb_C3 <- stemnb_C3[ order(names(stemnb_C3))]
    NAMES <- stem_nb[["CPP"]]
    row.names(stemnb_C3) <- NAMES
    
    q0 <- hill_taxa(stemnb_C3, q = 0)
    q1 <- hill_taxa(stemnb_C3, q = 1)
    q2 <- hill_taxa(stemnb_C3, q = 2)
    
  } else if(exists("idp", stem_nb)) {
    
    stemnb_C4 <- stem_nb[,c(-1,-57)]
    stemnb_C4 <- stemnb_C4[ order(names(stemnb_C4))]
    NAMES <- stem_nb[["idp"]]
    row.names(stemnb_C4) <- NAMES
    
    q0 <- hill_taxa(stemnb_C4, q = 0)
    q1 <- hill_taxa(stemnb_C4, q = 1)
    q2 <- hill_taxa(stemnb_C4, q = 2)
  }
  
  return(list(q0,q1,q2))
}

