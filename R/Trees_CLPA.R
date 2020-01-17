## Séléction des arbres appartenants aux placettes contenues 
## dans les polygones de la CLPA 

## (jusqu'en 2015 pour le Cycle 4)


trees_CLPA_C2 <- function(dataIFN,dataPlotCLPA){
  
  require(drake)
  require(dplyr)
  
  names(dataIFN) <- c("CODE_DEP","DEPARTEMENT","CYCLE","ANNEE_DE_REFERENCE",
                      "IDENTIFIANT_DU_POINT","NUMERO_ARBRE","CODE_ESSENCE",
                      "CODE_SOUS-UNITE","CODE_DOM","AGE","CIRCONFERENCE_A_LA_SOUCHE_cm",
                      "DIAMETRE_A_1.30_m_cm","HAUTEUR_TOTALE_m",
                      "ACCROISSEMENT_RADIAL_IR5_mm","ACCROISSEMENT_RADIAL_IR10_mm",
                      "VOLUME_HORS_REBUT_m^2","ACCROISSEMENT_DE_L'ARBRE_m^2.an^-1",
                      "PONDERATION")
  
  filtered_data <- dplyr::filter(dataIFN, dataIFN$IDENTIFIANT_DU_POINT %in% dataPlotCLPA$IDENTIFIANT.DU.POINT )

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
  
  return(filtered_data)
  }











