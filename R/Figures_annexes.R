Figure_annexe <- function(final_data_C4, basal_area_C4, DQ_C4, traits_plotC4, stem_nb_C4){
  
  require(drake)
  require(dplyr)
  require(stringr)
  require(reshape2)
  require(ggplot2)
  
  basal_area_C4$idp <- as.numeric(as.character(basal_area_C4$idp))
  DQ_C4$idp <- as.numeric(as.character(DQ_C4$idp))
  stem_nb_C4$idp <- as.numeric(as.character(stem_nb_C4$idp))
  
  full_var_C4 <- final_data_C4[[1]] 
  test <- left_join(full_var_C4,basal_area_C4[,c("idp","basal_area_tot")], by = "idp")
  test <- left_join(test, DQ_C4, by ="idp")
  test <- left_join(test, stem_nb_C4[,c("idp","stem_nb_tot")], by ="idp")
  
  plot_G_age <- ggplot(test, aes(mean_age,basal_area_tot)) 
  smooth_G_age <- plot_G_age + geom_smooth(data = test, mapping = aes(mean_age, basal_area_tot, group = avalanche_path, colour = avalanche_path), method = NULL, na.rm = TRUE, span = 1) +
    theme_bw() + theme(legend.position="bottom", legend.box = "horizontal",plot.title = element_text(hjust = 0.5)) + labs(color= "Avalanche", x ="Age", y = "Basal area") +
    scale_color_manual(labels = c("IN", "OUT"), values = c("red3", "darkblue"))
  
  plot_dG_age <- ggplot(test, aes(mean_age,RMS_diam)) 
  smooth_dG_age <- plot_dG_age + geom_smooth(data = test, mapping = aes(mean_age, RMS_diam, group = avalanche_path, colour = avalanche_path), method = NULL, na.rm = TRUE, span = 1) +
    theme_bw() + theme(legend.position="bottom", legend.box = "horizontal",plot.title = element_text(hjust = 0.5)) + labs(color= "Avalanche", x ="Age", y = "Root mean square diameter") +
    scale_color_manual(labels = c("IN", "OUT"), values = c("red3", "darkblue"))
 
  
  plot_stem_nb_age <- ggplot(test, aes(mean_age,stem_nb_tot)) 
  smooth_stem_nb_age <- plot_stem_nb_age + geom_smooth(data = test, mapping = aes(mean_age, stem_nb_tot, group = avalanche_path, colour = avalanche_path), method = NULL, na.rm = TRUE, span = 1) +
    theme_bw() + theme(legend.position="bottom", legend.box = "horizontal",plot.title = element_text(hjust = 0.5)) + labs(color= "Avalanche", x ="Age", y = "Stem number") +
    scale_color_manual(labels = c("IN", "OUT"), values = c("red3", "darkblue"))
  
  return(list(smooth_G_age,smooth_dG_age,smooth_stem_nb_age))
}

# 
# test <- figure_annexes(final_data_C4,basal_area_C4,DQ_C4,traits_plotC4,stem_nb_C4)
# G <- test[[1]]
# G
# 
# dG <- test[[2]]
# dG
# 
# st_nb <- test[[3]]
# st_nb
