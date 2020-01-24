## Séléction des arbres appartenants aux placettes contenues 
## dans les polygones de la CLPA 

## (jusqu'en 2015 pour le Cycle 4)


trees_CLPA_C2 <- function(dataIFN,dataPlotCLPA){
  
  require(drake)
  require(dplyr)
  
  filtered_data <- dplyr::filter(dataIFN, dataIFN$IDENTIFIANT_DU_POINT %in% dataPlotCLPA$IDENTIFIANT_DU_POINT )

  return(filtered_data)  
}

trees_CLPA_C3 <- function(dataIFN,dataPlotCLPA){
  
  require(drake)
  require(dplyr)
  
  filtered_data <- dplyr::filter(dataIFN, dataIFN$CPP %in% dataPlotCLPA$CPP )
  
  return(filtered_data)
  }

trees_CLPA_C4 <- function(dataIFN,dataPlotCLPA){
  
  require(drake)
  require(dplyr)
  
  filtered_data <- dplyr::filter(dataIFN, dataIFN$idp %in% dataPlotCLPA$idp )
  filtered_data$idp <- factor(filtered_data$idp)
  
  return(filtered_data)
  }











