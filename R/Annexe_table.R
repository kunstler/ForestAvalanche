annexe_table <- function(final_data_C4){
  
  library(drake)
  library(stringr)
  library(dplyr)
  library(rsq) # R^2 des glm
  library(performance) #comparaison des modeles
  library(car)
  library(knitr)
  
  full_var_C4 <- final_data_C4[[1]]
  full_var_C4_shannon <- final_data_C4[[2]]
  
  formule_log_SLA <- formula(log_SLA ~ Bio01_sc + pH_sc + Bio17_sc  + Bio04_sc + 
                               elevation_sc + elevation_sc*poly(mean_age,2) +
                               +avalanche_path*poly(mean_age,2) +
                               + elevation_sc*avalanche_path)
  
  formule_log_seed_mass <- formula(log_seed_mass ~ Bio01_sc + pH_sc + Bio17_sc  + Bio04_sc + 
                                     elevation_sc + elevation_sc*poly(mean_age,2)
                                   +avalanche_path*poly(mean_age,2)
                                   + elevation_sc*avalanche_path)
  
  formule_log_Wood_density <- formula(log_Wood_density ~ Bio01_sc + pH_sc + Bio17_sc  + Bio04_sc + 
                                        elevation_sc + elevation_sc*poly(mean_age,2)
                                      +avalanche_path*poly(mean_age,2)
                                      + elevation_sc*avalanche_path)
  
  formule_log_max_height <- formula(log_max_height ~ Bio01_sc + pH_sc + Bio17_sc  + Bio04_sc + 
                                      elevation_sc +  elevation_sc*poly(mean_age,2)
                                    +avalanche_path*poly(mean_age,2)
                                    + elevation_sc*avalanche_path)
  
  formule_rich_spe <- formula(hill_Q0 ~ Bio01_sc + pH_sc + Bio17_sc + elevation_sc + Bio04_sc + 
                                elevation_sc + elevation_sc*mean_age + elevation_sc*log(mean_age)
                              + avalanche_path + avalanche_path*mean_age + avalanche_path*log(mean_age))
  
  formule_shannon_log <- formula(hill_Q1_log ~ Bio01_sc + pH_sc + Bio17_sc + elevation_sc + 
                                   Bio04_sc + elevation_sc + elevation_sc*mean_age + 
                                   elevation_sc*log(mean_age) + avalanche_path + avalanche_path*mean_age +
                                   avalanche_path*log(mean_age))
  
  
  log_SLA_normale <- glm(formule_log_SLA, data = full_var_C4, family = gaussian())
  seed_mass_log_normale <- glm(formule_log_seed_mass, data = full_var_C4, family = gaussian())
  Wood_density_log_normale <- glm(formule_log_Wood_density, data = full_var_C4, family = gaussian())
  max_height_log_normale <- glm(formule_log_max_height, data = full_var_C4, family = gaussian())
  rich_poisson <- glm(formule_rich_spe , data = full_var_C4, family = poisson())
  shannon_normal_log <- glm(formule_shannon_log, data = full_var_C4_shannon, family = gaussian())
  
  ano_SLA <- Anova(log_SLA_normale)
  ano_seed <- Anova(seed_mass_log_normale)
  ano_density <- Anova(Wood_density_log_normale)
  ano_height<- Anova(max_height_log_normale)
  ano_rich <- Anova(rich_poisson)
  ano_shan <- Anova(shannon_normal_log)
  

  ano_CWM <- cbind(ano_SLA,ano_seed,ano_height,ano_density)
  table_traits <- kable(ano_CWM, digits = 3)
  
  ano_rich_shan <- cbind(ano_rich,ano_shan)
  table_taxo <- kable(ano_rich_shan, digits = 3)
  
  return(list(table_taxo,table_traits))
  
}
