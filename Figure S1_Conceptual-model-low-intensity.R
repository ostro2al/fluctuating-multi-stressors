#Figure S1 (supplementary materials)
#When intensity drops to 0 during moderate intensity stressor fluctuations [so staticstatic = 1 , fluc 0-2]
library(patchwork)
library(ggplot2)
library(tidyr)


theme_set(theme_classic())

#stressor matrices, 
#rows are different hypotheses
# columns are different stressor applications
x1 <- rbind(rep(1, 4),
            rep(1,4),
            c(0,2,0,2),
            c(0,2,0,2))

x2 <- rbind(rep(1, 4),
            c(0,2,0,2),
            c(0,2,0,2),
            c(2,0,2,0))

#effect sizes
a <- -3
b <- -2.5
c <- c(-1, 0, 1) #interaction
h1 <- h2 <- h3 <- data.frame(static_static = rep(NA, 3),
                             static_flux = NA,
                             sync = NA,
                             async = NA)

for (j in 1:4){#loop over applications
  #H1
  h1[,j] <- 4*(a*mean(x1[j,]) + b*mean(x2[j,]) + c*mean(x1[j,])*mean(x2[j,]))
  #H2
  h2[,j] <- 4*(a*max(x1[j,]) + b*max(x2[j,]) + c*max(x1[j,])*max(x2[j,]))
  # H3
  for (i in 1:3) h3[i,j] <- sum(x1[j,]*a + x2[j,]*b + x1[j,]*x2[j,]*c[i])
}

app_order <- c("static_static",
               "static_flux",
               "sync",
               "async")
h1$interaction <- h2$interaction <- h3$interaction <- c("synergism", "additive", "antagonism")  
h1 <- h1 %>% pivot_longer(-interaction,names_to = "application", values_to = "impact")
h1$application <- factor(h1$application, levels = app_order)
h2 <- h2 %>% pivot_longer(-interaction,names_to = "application", values_to = "impact")
h2$application <- factor(h2$application, levels = app_order)
h3 <- h3 %>% pivot_longer(-interaction,names_to = "application", values_to = "impact")
h3$application <- factor(h3$application, levels = app_order)

hall <- rbind(h1, h2, h3)
hall$hypo <- rep(c("H1", "H2", "H3"), each = nrow(h1))
#
# Plots
#
#Conversions so can plot with ggplot2
dstressors <- rbind(data.frame(t(x1)),
                    data.frame(t(x2+0.1)))             
dstressors$stressor <- rep(c("A", "B"), each = 4)
dstressors$t <- rep(1:4, 2)
dstressors <- dstressors %>% pivot_longer(-c(stressor,t), 
                                          names_to = "Application",
                                          values_to = "val")

g1 <- ggplot(dstressors) + 
  aes(x = t, y = val, col = stressor) + 
  geom_line() + 
  geom_point() + 
  ylab("Stressor intensity") +
  xlab("Period") +
  ylim(0, 4) + 
  facet_grid(.~Application) + 
  theme(text = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 12, colour = "black", face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank()
  )


ghall_int <- ggplot(hall) + 
  geom_col(aes(x = hypo, y = impact, fill = hypo)) +
  geom_text(aes(x = hypo, y = impact, label = impact), vjust = -0.5, colour = "white", size = 5)+
  facet_grid(interaction~application, scales = "free")+ 
  geom_hline(yintercept=0, linetype="solid", color = "black")+
  ylab("Impact (e.g., change in biomass)") +
  scale_fill_discrete(name = "Alternate hypotheses")+
  theme(text = element_text(size = 14, colour = "black", face = "bold"),
        axis.text = element_text(size = 12, colour = "black", face = "bold"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
  )

g1/ghall_int + plot_layout(heights = c(0.3, 1))
