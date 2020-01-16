## Séléction des arbres appartenants aux placettes contenues 
## dans les polygones de la CLPA 

## (jusqu'en 2015 pour le Cycle 4)


trees_CLPA_C2 <- function(dataIFN=treesC2,dataPlotCLPA = plot_IFN2_CLPA){
  
  require(drake)
  require(dplyr)
  
  names(dataIFN) <- c("CODE_DEP","DEPARTEMENT","CYCLE","ANNEE_DE_REFERENCE",
                      "IDENTIFIANT_DU_POINT","NUMERO_ARBRE","CODE_ESSENCE",
                      "CODE_SOUS-UNITE","CODE_DOM","AGE","CIRCONFERENCE_A_LA_SOUCHE_(cm)",
                      "DIAMETRE_A_1.30_m_(cm)","HAUTEUR_TOTALE_(m)",
                      "ACCROISSEMENT_RADIAL_IR5_(mm)","ACCROISSEMENT_RADIAL_IR10_(mm)",
                      "VOLUME_HORS_REBUT_(m^2)","ACCROISSEMENT_DE_L'ARBRE_(m^2/an)",
                      "PONDERATION")
  
  dplyr::filter(dataIFN, dataIFN$IDENTIFIANT_DU_POINT %in% dataPlotCLPA$IDENTIFIANT.DU.POINT )}

trees_CLPA_C3 <- function(dataIFN=treesC3,dataPlotCLPA = plot_IFN3_CLPA){
  
  require(drake)
  require(dplyr)
  
  dplyr::filter(dataIFN, dataIFN$CPP %in% dataPlotCLPA$CPP )}

trees_CLPA_C4 <- function(dataIFN=treesC4,dataPlotCLPA = plot_IFN4_CLPA){
  
  require(drake)
  require(dplyr)
  
  dplyr::filter(dataIFN, dataIFN$idp %in% dataPlotCLPA$idp )}











