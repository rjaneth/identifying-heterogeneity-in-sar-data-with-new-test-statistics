rm(list = ls())
library(ggplot2)
library(gtools)
library(stats4)
library(rmutil)
#library(gsl)
library(invgamma)
library(ggplot2)
library(ggthemes)
theme_set(theme_pander() +
            theme(text=element_text(family="serif"),
                  legend.position = "top")
)
source("../MainFunctions/gamma_sar_sample.r")
source("../MainFunctions/entropy_gamma_sar.r")

source("../MainFunctions/van_es_estimator.r")
source("../MainFunctions/correa_estimator.r")
source("../MainFunctions/ebrahimi_estimator.r")
source("../MainFunctions/noughabi_arghami_estimator.r")
source("../MainFunctions/vasicek_estimator.r")
source("../MainFunctions/al_omari_1_estimator.r")
source("../MainFunctions/al_omari_2_estimator.r")

set.seed(1234567890, kind="Mersenne-Twister")

sample_sizes <- c( 9, 25, 49, 81, 121, 1000)
R <- 100
mu <- 1
L <- 1

#  True entropy value for the gamma SAR distribution
true_entropy <- entropy_gamma_sar(L, mu)


output <- data.frame(SampleSize = integer(0), Estimator = character(0), Bias = numeric(0))

#List of estimator
estimators <- list(
  list(func = van_es_estimator, label = "Van Es"),
  list(func = correa_estimator, label = "Correa"),
  list(func = ebrahimi_estimator, label = "Ebrahimi"),
  list(func = noughabi_arghami_estimator, label = "Noughabi Arghami"),
  list(func = vasicek_estimator, label = "Vasicek"),
  list(func = al_omari_1_estimator, label = "Al Omari 1"),
  list(func = al_omari_2_estimator, label = "Al Omari 2")
)




for (estimator in estimators) {
  for (ssize in sample_sizes) {
    v.nonparametric.entropy <- numeric(R)
   
    for (r in 1:R) {
      sample <- gamma_sar_sample(L, mu, ssize)
      #cat("sample:", estimator$label, sample, "\n")
      v.nonparametric.entropy[r] <- estimator$func(sample)
      #cat("non:", v.nonparametric.entropy, "\n")
      
     
    }
    bias <- mean(v.nonparametric.entropy) - true_entropy
    # Calculate the bias 
    
    #cat("bias:", bias, "\n")
    
    output <- rbind(output, data.frame(SampleSize = ssize, Estimator = estimator$label, Bias = bias))
    cat("Estimator:", estimator$label, "Sample Size:", ssize, "Bias:", bias, "\n")
  }
}

# Plot the bias as a function of the sample size 
ggplot(output, aes(x = SampleSize, y = Bias, color = Estimator)) +
  geom_line() +
  geom_point() +
  xlab("Sample Size") +
  ylab(expression(paste("Bias ")))
#ylab(expression(paste("Bias ", (widehat(H)))))