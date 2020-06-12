Figure_rich_shann_C4 <- function(data = final_data_C4){

  require(drake)
  require(dplyr)
  require(ggplot2)

  # Appel des donn?es
  full_var_C4 <- data[[1]]
  full_var_C4_shannon <- data[[2]]
  
  
  # Formules des GLM
  formule_rich_spe <- formula(hill_Q0 ~ Bio01_sc + pH_sc + Bio17_sc + Bio04_sc + 
                                elevation_sc + elevation_sc*mean_age + elevation_sc*log(mean_age)
                              + avalanche_path + avalanche_path*mean_age + avalanche_path*log(mean_age))
  
  formule_shannon_log <- formula(hill_Q1_log ~ Bio01_sc + pH_sc + Bio17_sc + 
                                   Bio04_sc + elevation_sc + elevation_sc*mean_age + 
                                   elevation_sc*log(mean_age) + avalanche_path +
                                   avalanche_path*mean_age  + avalanche_path*log(mean_age))
  
  # GLM :
  rich_poisson <- glm(formule_rich_spe , data = full_var_C4, family = poisson())
  
  shannon_normal_log <- glm(formule_shannon_log, data = full_var_C4_shannon, family = gaussian())

  # Preparations des donnees pour prediction
  ## Species richness
  data_rich_1000_IN <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
                                   pH_sc = mean(full_var_C4$pH_sc),
                                   Bio17_sc = mean(full_var_C4$Bio17_sc),
                                   Bio04_sc = mean(full_var_C4$Bio04_sc),
                                   elevation_sc = c((1000- mean(full_var_C4[which(
                                     full_var_C4$avalanche_path == "IN"),"elevation"])) / 
                                       sd(full_var_C4[which(
                                         full_var_C4$avalanche_path == "IN"),"elevation"])), 
                                   mean_age = seq(quantile(full_var_C4[which(
                                     full_var_C4$avalanche_path == "IN"),"mean_age"], probs = 0.025),
                                     quantile(full_var_C4[which(
                                       full_var_C4$avalanche_path == "IN"),"mean_age"], probs = 0.975), 
                                     length.out = 300),
                                   avalanche_path = c("IN"))
  
  data_rich_1000_OUT <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
                                    pH_sc = mean(full_var_C4$pH_sc),
                                    Bio17_sc = mean(full_var_C4$Bio17_sc),
                                    Bio04_sc = mean(full_var_C4$Bio04_sc),
                                    elevation_sc = c((1000- mean(full_var_C4[which(
                                      full_var_C4$avalanche_path == "OUT"),"elevation"])) /
                                        sd(full_var_C4[which(
                                          full_var_C4$avalanche_path == "OUT"),"elevation"])), 
                                    mean_age = seq(quantile(full_var_C4[which(
                                      full_var_C4$avalanche_path == "OUT"),"mean_age"], probs = 0.025),
                                      quantile(full_var_C4[which(
                                        full_var_C4$avalanche_path == "OUT"),"mean_age"], probs = 0.975), 
                                      length.out = 300),
                                    avalanche_path = c("OUT"))
  
  
  data_rich_1700_IN <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
                                   pH_sc = mean(full_var_C4$pH_sc),
                                   Bio17_sc = mean(full_var_C4$Bio17_sc),
                                   Bio04_sc = mean(full_var_C4$Bio04_sc),
                                   elevation_sc = c((1700 - mean(full_var_C4[which(
                                     full_var_C4$avalanche_path == "IN"),"elevation"])) / 
                                       sd(full_var_C4[which(
                                         full_var_C4$avalanche_path == "IN"),"elevation"])), 
                                   mean_age = seq(quantile(full_var_C4[which(
                                     full_var_C4$avalanche_path == "IN"),"mean_age"], probs = 0.025),
                                     quantile(full_var_C4[which(
                                       full_var_C4$avalanche_path == "IN"),"mean_age"], probs = 0.975), 
                                     length.out = 300),
                                   avalanche_path = c("IN"))
  
  data_rich_1700_OUT <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
                                    pH_sc = mean(full_var_C4$pH_sc),
                                    Bio17_sc = mean(full_var_C4$Bio17_sc),
                                    Bio04_sc = mean(full_var_C4$Bio04_sc),
                                    elevation_sc = c((1700- mean(full_var_C4[which(
                                      full_var_C4$avalanche_path == "OUT"),"elevation"])) / 
                                        sd(full_var_C4[which(
                                          full_var_C4$avalanche_path == "OUT"),"elevation"])), 
                                    mean_age = seq(quantile(full_var_C4[which(
                                      full_var_C4$avalanche_path == "OUT"),"mean_age"], probs = 0.025),
                                      quantile(full_var_C4[which(
                                        full_var_C4$avalanche_path == "OUT"),"mean_age"], probs = 0.975), 
                                      length.out = 300),
                                    avalanche_path = c("OUT"))
  ## Shannon
  data_shannon_1000_IN <- expand.grid(Bio01_sc = mean(full_var_C4_shannon$Bio01_sc), 
                                      pH_sc = mean(full_var_C4_shannon$pH_sc),
                                      Bio17_sc = mean(full_var_C4_shannon$Bio17_sc),
                                      Bio04_sc = mean(full_var_C4_shannon$Bio04_sc),
                                      elevation_sc = c((1000- mean(full_var_C4_shannon[which(
                                        full_var_C4_shannon$avalanche_path == "IN"),"elevation"])) / 
                                          sd(full_var_C4_shannon[which(
                                            full_var_C4_shannon$avalanche_path == "IN"),"elevation"])), 
                                      mean_age = seq(quantile(full_var_C4_shannon[which(
                                        full_var_C4_shannon$avalanche_path == "IN"),"mean_age"], probs = 0.025),
                                        quantile(full_var_C4_shannon[which(
                                          full_var_C4_shannon$avalanche_path == "IN"),"mean_age"], probs = 0.975), 
                                        length.out = 300),
                                      avalanche_path = c("IN"))
  
  data_shannon_1000_OUT <- expand.grid(Bio01_sc = mean(full_var_C4_shannon$Bio01_sc), 
                                       pH_sc = mean(full_var_C4_shannon$pH_sc),
                                       Bio17_sc = mean(full_var_C4_shannon$Bio17_sc),
                                       Bio04_sc = mean(full_var_C4_shannon$Bio04_sc),
                                       elevation_sc = c((1000- mean(full_var_C4_shannon[which(
                                         full_var_C4_shannon$avalanche_path == "OUT"),"elevation"])) /
                                           sd(full_var_C4_shannon[which(
                                             full_var_C4_shannon$avalanche_path == "OUT"),"elevation"])), 
                                       mean_age = seq(quantile(full_var_C4_shannon[which(
                                         full_var_C4_shannon$avalanche_path == "OUT"),"mean_age"], probs = 0.025),
                                         quantile(full_var_C4_shannon[which(
                                           full_var_C4_shannon$avalanche_path == "OUT"),"mean_age"], probs = 0.975), 
                                         length.out = 300),
                                       avalanche_path = c("OUT"))
  
  
  data_shannon_1700_IN <- expand.grid(Bio01_sc = mean(full_var_C4_shannon$Bio01_sc), 
                                      pH_sc = mean(full_var_C4_shannon$pH_sc),
                                      Bio17_sc = mean(full_var_C4_shannon$Bio17_sc),
                                      Bio04_sc = mean(full_var_C4_shannon$Bio04_sc),
                                      elevation_sc = c((1700 - mean(full_var_C4_shannon[which(
                                        full_var_C4_shannon$avalanche_path == "IN"),"elevation"])) / 
                                          sd(full_var_C4_shannon[which(
                                            full_var_C4_shannon$avalanche_path == "IN"),"elevation"])), 
                                      mean_age = seq(quantile(full_var_C4_shannon[which(
                                        full_var_C4_shannon$avalanche_path == "IN"),"mean_age"], probs = 0.025),
                                        quantile(full_var_C4_shannon[which(
                                          full_var_C4_shannon$avalanche_path == "IN"),"mean_age"], probs = 0.975), 
                                        length.out = 300),
                                      avalanche_path = c("IN"))
  
  data_shannon_1700_OUT <- expand.grid(Bio01_sc = mean(full_var_C4_shannon$Bio01_sc), 
                                       pH_sc = mean(full_var_C4_shannon$pH_sc),
                                       Bio17_sc = mean(full_var_C4_shannon$Bio17_sc),
                                       Bio04_sc = mean(full_var_C4_shannon$Bio04_sc),
                                       elevation_sc = c((1700- mean(full_var_C4_shannon[which(
                                         full_var_C4_shannon$avalanche_path == "OUT"),"elevation"])) / 
                                           sd(full_var_C4_shannon[which(
                                             full_var_C4_shannon$avalanche_path == "OUT"),"elevation"])), 
                                       mean_age = seq(quantile(full_var_C4_shannon[which(
                                         full_var_C4_shannon$avalanche_path == "OUT"),"mean_age"], probs = 0.025),
                                         quantile(full_var_C4_shannon[which(
                                           full_var_C4_shannon$avalanche_path == "OUT"),"mean_age"], probs = 0.975), 
                                         length.out = 300),
                                       avalanche_path = c("OUT"))
  
  # Predictions des donn?es par les GLM
  
  
## Species richness
  pred_rich_1000_IN <- predict(rich_poisson, newdata = data_rich_1000_IN, type = c("response"))
  data_rich_1000_IN$predited_value <- pred_rich_1000_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(rich_poisson, newdata = data_rich_1000_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- rich_poisson$family$linkinv(upr)
  lwr2 <- rich_poisson$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_rich_1000_IN$lwr <- lwr2 
  data_rich_1000_IN$upr <- upr2 
  data_rich_1000_IN$type <- "a) Species richness"
  data_rich_1000_IN$altitude <- 1000
  
  pred_rich_1000_OUT <- predict(rich_poisson, newdata = data_rich_1000_OUT, type = c("response"))
  data_rich_1000_OUT$predited_value <- pred_rich_1000_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(rich_poisson, newdata = data_rich_1000_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- rich_poisson$family$linkinv(upr)
  lwr2 <- rich_poisson$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_rich_1000_OUT$lwr <- lwr2 
  data_rich_1000_OUT$upr <- upr2 
  data_rich_1000_OUT$type <- "a) Species richness"
  data_rich_1000_OUT$altitude <- 1000
  
  pred_rich_1700_IN <- predict(rich_poisson, newdata = data_rich_1700_IN, type = c("response"))
  data_rich_1700_IN$predited_value <- pred_rich_1700_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(rich_poisson, newdata = data_rich_1700_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- rich_poisson$family$linkinv(upr)
  lwr2 <- rich_poisson$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_rich_1700_IN$lwr <- lwr2 
  data_rich_1700_IN$upr <- upr2 
  data_rich_1700_IN$type <- "a) Species richness"
  data_rich_1700_IN$altitude <- 1700
  
  
  pred_rich_1700_OUT <- predict(rich_poisson, newdata = data_rich_1700_OUT, type = c("response"))
  data_rich_1700_OUT$predited_value <- pred_rich_1700_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(rich_poisson, newdata = data_rich_1700_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- rich_poisson$family$linkinv(upr)
  lwr2 <- rich_poisson$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_rich_1700_OUT$lwr <- lwr2 
  data_rich_1700_OUT$upr <- upr2 
  data_rich_1700_OUT$type <- "a) Species richness"
  data_rich_1700_OUT$altitude <- 1700
  

  ## Shannon
  pred_shannon_1000_IN <- predict(shannon_normal_log, newdata = data_shannon_1000_IN, type = c("response"))
  data_shannon_1000_IN$predited_value <- pred_shannon_1000_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(shannon_normal_log, newdata = data_shannon_1000_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- shannon_normal_log$family$linkinv(upr)
  lwr2 <- shannon_normal_log$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_shannon_1000_IN$lwr <- lwr2 
  data_shannon_1000_IN$upr <- upr2 
  data_shannon_1000_IN$type <- "b) Shannon index"
  data_shannon_1000_IN$altitude <- 1000
  
  pred_shannon_1000_OUT <- predict(shannon_normal_log, newdata = data_shannon_1000_OUT, type = c("response"))
  data_shannon_1000_OUT$predited_value <- pred_shannon_1000_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(shannon_normal_log, newdata = data_shannon_1000_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- shannon_normal_log$family$linkinv(upr)
  lwr2 <- shannon_normal_log$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_shannon_1000_OUT$lwr <- lwr2 
  data_shannon_1000_OUT$upr <- upr2 
  data_shannon_1000_OUT$type <- "b) Shannon index"
  data_shannon_1000_OUT$altitude <- 1000
  
  pred_shannon_1700_IN <- predict(shannon_normal_log, newdata = data_shannon_1700_IN, type = c("response"))
  data_shannon_1700_IN$predited_value <- pred_shannon_1700_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(shannon_normal_log, newdata = data_shannon_1700_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- shannon_normal_log$family$linkinv(upr)
  lwr2 <- shannon_normal_log$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_shannon_1700_IN$lwr <- lwr2 
  data_shannon_1700_IN$upr <- upr2 
  data_shannon_1700_IN$type <- "b) Shannon index"
  data_shannon_1700_IN$altitude <- 1700
  
  pred_shannon_1700_OUT <- predict(shannon_normal_log, newdata = data_shannon_1700_OUT, type = c("response"))
  data_shannon_1700_OUT$predited_value <- pred_shannon_1700_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(shannon_normal_log, newdata = data_shannon_1700_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregr? de libert? tr?s grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- shannon_normal_log$family$linkinv(upr)
  lwr2 <- shannon_normal_log$family$linkinv(lwr)
  
  # tout dans un tableau de donn?e:
  data_shannon_1700_OUT$lwr <- lwr2 
  data_shannon_1700_OUT$upr <- upr2 
  
  data_shannon_1700_OUT$type <- "b) Shannon index"
  data_shannon_1700_OUT$altitude <- 1700
  
  
  
  # Assemblage des pr?dictions
  ## Richesse sp?cifique
  rich_1000 <- bind_rows(data_rich_1000_IN,data_rich_1000_OUT)
  rich_1700 <- bind_rows(data_rich_1700_IN,data_rich_1700_OUT)
  rich <- bind_rows(rich_1000,rich_1700)
  
  ## Shannon
  shannon_1000 <- bind_rows(data_shannon_1000_IN,data_shannon_1000_OUT)
  shannon_1700 <- bind_rows(data_shannon_1700_IN,data_shannon_1700_OUT)
  shann <- bind_rows(shannon_1000,shannon_1700)
  
  
  # Mise en forme de la figure
  
  complet <- bind_rows(rich,shann)
  
  plot_rich_shan <- ggplot(data=complet, mapping=aes(x=mean_age ,y=predited_value,  group = avalanche_path, colour = avalanche_path)) + geom_point(size = 0.7)  + 
    theme_bw() + 
    theme(legend.position="bottom", legend.box = "horizontal",plot.title = element_text(hjust = 0.5)) +
    labs(color= "Avalanche", x ="Age") + scale_color_manual(labels = c("IN", "OUT"), values = c("red3", "darkblue")) +
    geom_line(data=complet[which(complet$avalanche_path == "IN"),], mapping=aes(x=mean_age, y=upr), col="red", linetype = "longdash") + 
    geom_line(data=complet[which(complet$avalanche_path == "IN"),], mapping=aes(x=mean_age, y=lwr), col="red", linetype = "longdash") + 
    geom_line(data=complet[which(complet$avalanche_path == "OUT"),], mapping=aes(x=mean_age, y=upr), col="blue4", linetype = "longdash") + 
    geom_line(data=complet[which(complet$avalanche_path == "OUT"),], mapping=aes(x=mean_age, y=lwr), col="blue4", linetype = "longdash") + facet_grid(type ~ altitude, scales = "free")
  
  return(plot_rich_shan)

}
