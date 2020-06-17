## Séléction des arbres appartenants aux placettes contenues 
## dans les polygones de la CLPA 

## (jusqu'en 2015 pour le Cycle 4)


trees_CLPA_C4 <- function(dataIFN,dataPlotCLPA){
  
  require(drake)
  require(dplyr)
  
  filtered_data <- dplyr::filter(dataIFN, dataIFN$idp %in% dataPlotCLPA$idp )
  filtered_data$idp <- factor(filtered_data$idp)
  
  return(filtered_data)
  }











