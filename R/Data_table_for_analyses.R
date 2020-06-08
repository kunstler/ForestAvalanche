data_analyse_C4 <- function(FD_C4,plotsC4,traits_plotC4,taxo_divC4, list_all){

  require(drake)
  require(dplyr)

envC4 <- plotsC4[,c("idp","avalanche_path","Bio01","Bio04","Bio17","pH","pent2",
                    "altitude","mean_age")]
envC4$avalanche_path <- as.factor(envC4$avalanche_path)

ecology_C4 <- list_all[["ecologie"]]
ecology_C4$idp <- as.numeric(ecology_C4$idp)
topo_C4 <- ecology_C4[which(ecology_C4$idp %in% plotsC4$idp),c("idp","pent2")]
topo_C4 <- subset(topo_C4, !is.na(topo_C4$pent2))

CWM_C4 <- FD_C4[["CWM"]]
CWM_C4$idp <- as.numeric(row.names(CWM_C4))
CWM_C4_seleted <- CWM_C4[,c("idp","SLA","Wood_density","mean_height","mean_seed_mass","log_SLA",
                            "log_Wood_density","log_max_height","log_seed_mass")]
CWM_C4_seleted <- CWM_C4_seleted[which(CWM_C4_seleted$idp %in% topo_C4$idp),]

hill_C4 <- data.frame(taxo_divC4[[1]])
hill_C4$Q1 <- taxo_divC4[[2]]
hill_C4$Q2 <- taxo_divC4[[2]]
names(hill_C4) <- c("hill_Q0","hill_Q1","hill_Q2")
hill_C4$idp <- as.numeric(row.names(hill_C4))

plots_glm_C4 <- left_join(CWM_C4_seleted,envC4, by = "idp")
plots_glm_C4 <- left_join(plots_glm_C4, hill_C4[,c("hill_Q0","hill_Q1","idp")], by = "idp")
plots_glm_C4 <- plots_glm_C4[which(plots_glm_C4$idp %in% topo_C4$idp),]
names(plots_glm_C4) <- c("idp","SLA","Wood_density","max_height","seed_mass","log_SLA",
                         "log_Wood_density","log_max_height","log_seed_mass","avalanche_path",
                         "Bio01","Bio04","Bio17","pH","slope","elevation","mean_age",
                         "hill_Q0", "hill_Q1")

#centrer réduire les variables quantitatives expliquatives

centre_reduit <- scale(plots_glm_C4[,c(11:17)])
centre_reduit <- data.frame(centre_reduit)
centre_reduit$idp <- plots_glm_C4$idp
names(centre_reduit) <- c("Bio01_sc","Bio04_sc","Bio17_sc","pH_sc","slope_sc",
                          "elevation_sc","mean_age_sc", "idp")
scale_varC4 <- left_join(plots_glm_C4[,c("idp","hill_Q0","hill_Q1","SLA","Wood_density",
                                         "max_height","seed_mass","log_SLA","log_Wood_density",
                                         "log_max_height","log_seed_mass","avalanche_path")],
                         centre_reduit, by = "idp")

# assemblage des 2 tableaux

full_var_C4 <- left_join(scale_varC4,plots_glm_C4[, c("idp","Bio01","Bio04","Bio17","pH","slope",
                                                      "elevation","mean_age")], by = "idp")
full_var_C4$hill_Q1_log <- log(full_var_C4$hill_Q1)

# sous sélection des plots de plus d'une espèce.

full_var_C4_shannon <- full_var_C4[which(full_var_C4$hill_Q1 > 1),]

return(list(full_var_C4,full_var_C4_shannon))

}