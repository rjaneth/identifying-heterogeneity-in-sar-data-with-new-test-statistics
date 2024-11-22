### Illustrations for the talk about "Identifying Heterogeneity in SAR Data with New Test Statistics"

source("~/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/Documents/MeusLivros/Book-OperationalStatisticsSAR/Codes/GammaSAR.R")
source("~/Library/CloudStorage/OneDrive-VictoriaUniversityofWellington-STAFF/Documents/Programas/R/GI0Package/GI0/R/dGI0.R")

library(ggplot2)
library(ggthemes)
library(latex2exp)

x <- seq(.0001, 5, by=0.001)

df <- data.frame(
  x,
  dgamma=dgammaSAR(x, 2, 1), # GammaSAR density, two looks, unitary mean
  dgi0.1.5=dGI0(x, -1.5, 0.5, 2), # GI0 density, two looks, alpha=-1.5, gamma=0.5
  dgi0.1.5=dGI0(x, -3, 2, 2), # GI0 density, two looks, alpha=-3, gamma=2
  dgi0.8=dGI0(x, -8, 7, 2) # GI0 density, two looks, alpha=-3, gamma=7
)

df.molten <- reshape2::melt(df, 
                            value.name = "Density", 
                            measure.vars = 2:5,
                            variable.name="Model")

legend <- c()

ggplot(df.molten, aes(x=x, y=Density, colour=Model)) +
  geom_vline(xintercept = 1, linetype=2, colour="darkgray") +
  geom_line(linewidth=1.3, alpha=.7) +
  scale_color_discrete(
    labels = c(
      TeX(r"($\Gamma_{{SAR}}(1,2)$)"), 
      TeX(r"($G_I^0(-1.5, 0.5, 2)$)"),  
      TeX(r"($G_I^0(-3, 2, 2)$)"),  
      TeX(r"($G_I^0(-8, 7, 2)$)")
    )
  ) +
  theme_clean() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'),   # Reduce the size of legend keys
        legend.text = element_text(size = 8), # Reduce the size of legend text
        legend.background = element_rect(fill=rgb(0.95, 0.95, 0.96)),
        panel.background = element_rect(fill = rgb(0.95, 0.95, 0.96), colour = "black"), # Panel background color
        plot.background = element_rect(fill = rgb(0.95, 0.95, 0.96))                # Plot background color
  )
ggsave(file="../../../Figures/PDF/GammaGI0Densities.pdf", 
       width = 12, height=10, units = "cm")


ggplot(df.molten, aes(x=x, y=Density, colour=Model)) +
  geom_vline(xintercept = 1, linetype=2, colour="darkgray") +
  geom_line(linewidth=1.3, alpha=.7) +
  scale_y_log10() +
  scale_color_discrete(
    labels = c(
      TeX(r"($\Gamma_{{SAR}}(1,2)$)"), 
      TeX(r"($G_I^0(-1.5, 0.5, 2)$)"),  
      TeX(r"($G_I^0(-3, 2, 2)$)"),  
      TeX(r"($G_I^0(-8, 7, 2)$)")
    )
  ) +
  theme_clean() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'),   # Reduce the size of legend keys
        legend.text = element_text(size = 8),
        legend.background = element_rect(fill=rgb(0.95, 0.95, 0.96)),
        panel.background = element_rect(fill = rgb(0.95, 0.95, 0.96), colour = "black"), # Panel background color
        plot.background = element_rect(fill = rgb(0.95, 0.95, 0.96))                # Plot background color
  )

ggsave(file="../../../Figures/PDF/GammaGI0DensitiesSemilog.pdf", 
       width = 12, height=10, units = "cm")
