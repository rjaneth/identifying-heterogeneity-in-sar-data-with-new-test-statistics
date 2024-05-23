
rm(list = ls())

library(ggplot2)
library(ggsci)
library(univariateML)
library(compiler)
library(MASS)
library(fitdistrplus)
library(invgamma)
library(gridExtra)

# source("../MainFunctions/ebrahimi_estimator.R")
# source("../MainFunctions/gamma_sar_sample.R")
# source("../MainFunctions/correa_estimator.R")
# source("../MainFunctions/bootstrap_correa_estimator.R")
# source("../MainFunctions/bootstrap_correa_estimator_log_mean.R")
# source("../MainFunctions/entropy_gI0.R")
# source("../MainFunctions/gi0_sample.R")



#load("../Programs/Data/results_data_simulated_Phantom_7.Rdata")
#save(cd_values_mnad, Z, file = "./Data/results_Phantom_mnad_7.Rdata")
#load("../Programs/Data/results_Phantom_mnad_7.Rdata")

#save( cv_values, x, file = "./Data/CV_results_data_Illinois_crops_1024.Rdata")
#load("../Programs/Data/CV_results_data_Illinois_crops_1024.Rdata")

#save( cd_values_mnad, x, file = "./Data/MnAD_results_data_Illinois_crops_1024.Rdata")
#load("../Programs/Data/MnAD_results_data_Illinois_crops_1024.Rdata")

#save( cv_values, x, file = "./Data/CV_results_data_Mexixo_512.Rdata")
#load("../Programs/Data/CV_results_data_Mexixo_512.Rdata")

#save( cd_values_mnad, x, file = "./Data/MnAD_results_data_mexico_512.Rdata")
#load("../Programs/Data/MnAD_results_data_mexico_512.Rdata")

#save( cv_values, x, file = "./Data/CV_results_data_lake_512.Rdata")
#load("../Programs/Data/CV_results_data_lake_512.Rdata")

#save( cd_values_mnad, x, file = "./Data/MnAD_results_data_lake_512.Rdata")
load("../Programs/Data/MnAD_results_data_lake_512.Rdata")
#cv_values_vector <- as.vector(cv_values)


cv_values_vector <- as.vector(cd_values_mnad)



fln <- fitdist(cv_values_vector, "lnorm")
fg <- fitdist(cv_values_vector, "gamma")
fn <- fitdist(cv_values_vector, "norm")
fw <- fitdist(cv_values_vector, "weibull")


summary(fln)
summary(fg)
summary(fn)
summary(fw)


par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot.legend <- c( "Lognormal", "Gamma", "Normal", "Weibull") 
num_bins <- 60 
density_plots <- list(
  denscomp(list(fln, fg, fn, fw), n = num_bins, legendtext = c("Lognormal", "Gamma", "Normal", "Weibull"), 
           fitcol = c("darkblue", "#56B4E9", "#FC4E07", "#009E80"), fitlwd = c(1.6, 1.3, 1.2, 1.0),
           fitlty = 1, xlab = expression(CV), plotstyle = "ggplot") +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    theme(text = element_text(family = "serif"), legend.position = "bottom"),
  
  qqcomp(list(fln, fg, fn, fw), legendtext = c("Lognormal", "Gamma", "Normal", "Weibull"), 
         fitcol = c("darkblue", "#56B4E9", "#FC4E07", "#009E80"), fitlwd = c(1.6, 1.3, 1.2, 1.0),
         plotstyle = "ggplot") +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    theme(text = element_text(family = "serif"), legend.position = "bottom"),
  
  cdfcomp(list(fln, fg, fn, fw), legendtext = c("Lognormal", "Gamma", "Normal", "Weibull"), 
          fitcol = c("darkblue", "#56B4E9", "#FC4E07", "#009E80"), fitlwd = c(1.6, 1.3, 1.2, 1.0),
          fitlty = 1, xlab = expression(CV), plotstyle = "ggplot") +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    theme(text = element_text(family = "serif"), legend.position = "bottom"),
  
  ppcomp(list(fln, fg, fn, fw), legendtext = c("Lognormal", "Gamma", "Normal", "Weibull"), 
         fitcol = c("darkblue", "#56B4E9", "#FC4E07", "#009E80"), fitlwd = c(1.6, 1.3, 1.2, 1.0),
         plotstyle = "ggplot") +
    geom_line(alpha = 0.7) +
    theme_minimal() +
    theme(text = element_text(family = "serif"), legend.position = "bottom")
)


combined_plot1 <- wrap_plots(density_plots, ncol = 2) +
  plot_layout(guides = "collect")

print(combined_plot1)

