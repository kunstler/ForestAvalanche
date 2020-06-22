functional_dispersion <- function(data = final_data_C4, basal_area_C4, traits_plotC4, height, seed_mass){
  
  require(drake)
  require(dplyr)
  require(stringr)
  require(reshape2)
  
  
  full_var_C4 <- data[[1]]
  height <- height[[2]]
  seed_mass <- seed_mass[[2]]
  
  basal_area_C4$idp <- as.numeric(as.character(basal_area_C4$idp))
  basal_area_filtered <- dplyr::filter(basal_area_C4, idp %in% full_var_C4$idp)
  esp <- names(basal_area_filtered)
  esp <- esp[-1]
  basal_area_grp <- reshape2::melt(basal_area_filtered, measure.vars = esp, variable.name = "Latin_Name" , value.name = "basal_area" )
  basal_area_grp[,"Latin_Name"] <- as.character(str_to_title(gsub("_", " ", basal_area_grp[,"Latin_Name"])))
  
  basal_area_grp <- basal_area_grp[which(basal_area_grp[,"basal_area"] != 0),]
  basal_area_grp <- basal_area_grp %>% group_by(idp, Latin_Name)
  
  basal_area_tot <- basal_area_filtered[,c("idp","basal_area_tot")]
  
  traits_plotC4 <- traits_plotC4[,c("idp","Code_esp", "Latin_Name", "SLA","Wood_density")]
  traits_plotC4$idp <- as.numeric(as.character(traits_plotC4$idp))
  traits_plotC4 <- left_join(traits_plotC4,basal_area_tot, by = "idp")
  
  height$Latin_Name <- str_to_title(gsub("_"," ",height$Latin_Name)) 
  height_grp <- height %>% dplyr::group_by(Latin_Name) %>% 
    dplyr::summarise(max_height = mean(mean_height, na.rm = TRUE))
  
  seed_mass$Latin_Name <- str_to_title(gsub("_"," ",seed_mass$Latin_Name)) 
  seed_mass_grp <- seed_mass %>% dplyr::group_by(Latin_Name) %>% 
    dplyr::summarise(seed_mass = mean(seed_mass_mean, na.rm = TRUE))
  
  traits_plotC4 <- dplyr::filter(traits_plotC4, idp %in% full_var_C4$idp)
  traits_plotC4 <- left_join(traits_plotC4, height_grp[,c("Latin_Name","max_height")], by = "Latin_Name")
  traits_plotC4 <- left_join(traits_plotC4, seed_mass_grp[,c("Latin_Name","seed_mass")], by = "Latin_Name")
  traits_plotC4 <- traits_plotC4[,-2]
  traits_plotC4 <- traits_plotC4[,c(1,2,3,7,6,4,5)]
  names(traits_plotC4) <- c("idp","Latin_Name","SLA","SM","Hmax","WD","basal_area_tot")
  traits_plotC4_grp <- traits_plotC4 %>% group_by(idp, Latin_Name) 
  traits_plotC4_grp <- left_join(traits_plotC4_grp, basal_area_grp, by = c("idp","Latin_Name"))
  
  full_var_short <- full_var_C4[,c("idp","SLA","seed_mass","max_height","Wood_density")]
  names(full_var_short) <- c("idp","CWM_SLA","CWM_SM","CWM_Hmax","CWM_WD")
  full_var_calc <- left_join(traits_plotC4_grp, full_var_short, by = "idp") 
  
  FDis_SLA <- full_var_calc %>% group_by(idp) %>% 
    dplyr::summarise(FDis_SLA = sum(basal_area*sqrt((SLA-CWM_SLA)^2)/basal_area_tot))
  FDis_SM <- full_var_calc %>% group_by(idp) %>% 
    dplyr::summarise(FDis_SM = sum(basal_area*sqrt((SM-CWM_SM)^2)/basal_area_tot))
  FDis_Hmax <- full_var_calc %>% group_by(idp) %>% 
    dplyr::summarise(FDis_Hmax = sum(basal_area*sqrt((Hmax-CWM_Hmax)^2)/basal_area_tot))
  FDis_WD <- full_var_calc %>% group_by(idp) %>% 
    dplyr::summarise(FDis_WD = sum(basal_area*sqrt((WD-CWM_WD)^2)/basal_area_tot))
  
  full_var_C4 <- left_join(full_var_C4,FDis_SLA, by = "idp")
  full_var_C4 <- left_join(full_var_C4,FDis_SM, by = "idp")
  full_var_C4 <- left_join(full_var_C4,FDis_Hmax, by = "idp")
  full_var_C4 <- left_join(full_var_C4,FDis_WD, by = "idp")
  
  return(full_var_C4)
}
