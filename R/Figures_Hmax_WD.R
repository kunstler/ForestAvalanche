Figure_Hmax_WD_C4 <- function(data = final_data_C4){

  require(drake)
  require(dplyr)
  require(ggplot2)

  # Appel des donn?es
  full_var_C4 <- data[[1]]
  
  
  # Formules des GLM
  formule_log_max_height <- formula(log_max_height ~ Bio01_sc + pH_sc + Bio17_sc  + Bio04_sc + 
                                      elevation_sc + elevation_sc*poly(mean_age,2) + 
                                      avalanche_path*poly(mean_age,2) +
                                      elevation_sc*avalanche_path)
  
  formule_log_Wood_density <- formula(log_Wood_density ~ Bio01_sc + pH_sc + Bio17_sc  + 
                                        Bio04_sc + elevation_sc + elevation_sc*poly(mean_age,2) +
                                        avalanche_path*poly(mean_age,2) +
                                        elevation_sc*avalanche_path)
  
  # GLM :
  Wood_density_log_normale <- glm(formule_log_Wood_density, data = full_var_C4, family = gaussian())
  max_height_log_normale <- glm(formule_log_max_height, data = full_var_C4, family = gaussian())

  # Preparations des donnees pour prediction
  data_1000_IN <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
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
  
  data_1000_OUT <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
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
  
  
  data_1700_IN <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
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
  
  data_1700_OUT <- expand.grid(Bio01_sc = mean(full_var_C4$Bio01_sc), 
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
  
  
  # Predictions des donn?es par les GLM
  
##Hmax
  pred_max_height_1000_IN <- predict(max_height_log_normale, newdata = data_1000_IN, type = c("response"))
  data_1000_IN$predicted_value <- pred_max_height_1000_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(max_height_log_normale, newdata = data_1000_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- max_height_log_normale$family$linkinv(upr)
  lwr2 <- max_height_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1000_IN$lwr <- lwr2 
  data_1000_IN$upr <- upr2 
  data_1000_IN$type <- "a) log(Hmax)"
  data_1000_IN$altitude <- 1000
  
  
  pred_max_height_1000_OUT <- predict(max_height_log_normale, newdata = data_1000_OUT, type = c("response"))
  data_1000_OUT$predicted_value <- pred_max_height_1000_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(max_height_log_normale, newdata = data_1000_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- max_height_log_normale$family$linkinv(upr)
  lwr2 <- max_height_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1000_OUT$lwr <- lwr2 
  data_1000_OUT$upr <- upr2 
  data_1000_OUT$type <- "a) log(Hmax)"
  data_1000_OUT$altitude <- 1000
  
  pred_max_height_1700_IN <- predict(max_height_log_normale, newdata = data_1700_IN, type = c("response"))
  data_1700_IN$predicted_value <- pred_max_height_1700_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(max_height_log_normale, newdata = data_1700_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- max_height_log_normale$family$linkinv(upr)
  lwr2 <- max_height_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1700_IN$lwr <- lwr2 
  data_1700_IN$upr <- upr2 
  data_1700_IN$type <- "a) log(Hmax)"
  data_1700_IN$altitude <- 1700
  
  pred_max_height_1700_OUT <- predict(max_height_log_normale, newdata = data_1700_OUT, type = c("response"))
  data_1700_OUT$predicted_value <- pred_max_height_1700_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(max_height_log_normale, newdata = data_1700_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- max_height_log_normale$family$linkinv(upr)
  lwr2 <- max_height_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1700_OUT$lwr <- lwr2 
  data_1700_OUT$upr <- upr2 
  data_1700_OUT$type <- "a) log(Hmax)"
  data_1700_OUT$altitude <- 1700
  
  # Assemblage des pr?dictions
  
## Hmax
  max_height_1000 <- bind_rows(data_1000_IN,data_1000_OUT)
  max_height_1700 <- bind_rows(data_1700_IN,data_1700_OUT)
  Hmax <- bind_rows(max_height_1000,max_height_1700)
  
##WD  
  pred_Wood_density_1000_IN <- predict(Wood_density_log_normale, newdata = data_1000_IN, type = c("response"))
  data_1000_IN$predicted_value <- pred_Wood_density_1000_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(Wood_density_log_normale, newdata = data_1000_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- Wood_density_log_normale$family$linkinv(upr)
  lwr2 <- Wood_density_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1000_IN$lwr <- lwr2 
  data_1000_IN$upr <- upr2 
  data_1000_IN$type <- "b) log(WD)"
  data_1000_IN$altitude <- 1000
  
  pred_Wood_density_1000_OUT <- predict(Wood_density_log_normale, newdata = data_1000_OUT, type = c("response"))
  data_1000_OUT$predicted_value <- pred_Wood_density_1000_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(Wood_density_log_normale, newdata = data_1000_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- Wood_density_log_normale$family$linkinv(upr)
  lwr2 <- Wood_density_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1000_OUT$lwr <- lwr2 
  data_1000_OUT$upr <- upr2 
  data_1000_OUT$type <- "b) log(WD)"
  data_1000_OUT$altitude <- 1000
  
  pred_Wood_density_1700_IN <- predict(Wood_density_log_normale, newdata = data_1700_IN, type = c("response"))
  data_1700_IN$predicted_value <- pred_Wood_density_1700_IN
  
  # pour le calcul des intervals de confiance :
  preds <- predict(Wood_density_log_normale, newdata = data_1700_IN, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- Wood_density_log_normale$family$linkinv(upr)
  lwr2 <- Wood_density_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1700_IN$lwr <- lwr2 
  data_1700_IN$upr <- upr2 
  data_1700_IN$type <- "b) log(WD)"
  data_1700_IN$altitude <- 1700
  
  pred_Wood_density_1700_OUT <- predict(Wood_density_log_normale, newdata = data_1700_OUT, type = c("response"))
  data_1700_OUT$predicted_value <- pred_Wood_density_1700_OUT
  
  # pour le calcul des intervals de confiance :
  preds <- predict(Wood_density_log_normale, newdata = data_1700_OUT, type = c("link"), se.fit =TRUE)
  
  ## approx 95% CI   Quantile de oi normale pour dregré de liberté très grand et intervalle de confiance a 95% : environ 1.96
  critval <- 1.96
  
  # calcul interval conf
  upr <- preds$fit + (critval * preds$se.fit)
  lwr <- preds$fit - (critval * preds$se.fit)
  
  # inverse de la fonction de liens
  upr2 <- Wood_density_log_normale$family$linkinv(upr)
  lwr2 <- Wood_density_log_normale$family$linkinv(lwr)
  
  # tout dans un tableau de donnée:
  data_1700_OUT$lwr <- lwr2 
  data_1700_OUT$upr <- upr2 
  data_1700_OUT$type <- "b) log(WD)"
  data_1700_OUT$altitude <- 1700
  
  # Assemblage des pr?dictions

## WD
  Wood_density_1000 <- bind_rows(data_1000_IN,data_1000_OUT)
  Wood_density_1700 <- bind_rows(data_1700_IN,data_1700_OUT)
  WD <- bind_rows(Wood_density_1000,Wood_density_1700)
  
  # Mise en forme de la figure
  
  complet <- bind_rows(Hmax,WD)
  
  plot_Hmax_WD <- ggplot(data=complet, mapping=aes(x=mean_age ,y=predicted_value,  group = avalanche_path, colour = avalanche_path)) + geom_point(size = 0.7)  + 
    theme_bw() + 
    theme(legend.position="bottom", legend.box = "horizontal",plot.title = element_text(hjust = 0.5)) +
    labs(color= "Avalanche", x ="Age") + scale_color_manual(labels = c("IN", "OUT"), values = c("red3", "darkblue")) +
    geom_line(data=complet[which(complet$avalanche_path == "IN"),], mapping=aes(x=mean_age, y=upr), col="red", linetype = "longdash") + 
    geom_line(data=complet[which(complet$avalanche_path == "IN"),], mapping=aes(x=mean_age, y=lwr), col="red", linetype = "longdash") + 
    geom_line(data=complet[which(complet$avalanche_path == "OUT"),], mapping=aes(x=mean_age, y=upr), col="blue4", linetype = "longdash") + 
    geom_line(data=complet[which(complet$avalanche_path == "OUT"),], mapping=aes(x=mean_age, y=lwr), col="blue4", linetype = "longdash") + facet_grid(type ~ altitude, scales = "free")
  
  return(plot_Hmax_WD)

}

