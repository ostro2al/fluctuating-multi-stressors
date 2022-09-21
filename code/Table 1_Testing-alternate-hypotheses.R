#Table 1 - Analysis of underlying mechanisms driving differential responses to variable stressor intensity and timing
#Analysis on proportional biomass change
#Interactive GAM with fixed degrees of freedom, calculating bias and RMSE of all models 
library(mgcv)
library(readr)
library(dplyr)
library(visreg)
library(ggplot2)

dat <- read.csv("Full_Dataset.csv")

unique(dat$Stressor_app)
datstatic <- filter(dat, Stressor_app == "StaticStatic")
datfluc <- filter(dat, Stressor_app != "StaticStatic")  

dat$Light_conc <- as.factor(dat$Light_conc)
dat$Diuron_conc <- as.factor(dat$Diuron_conc)
dat$Stressor_app <- as.factor(dat$Stressor_app)

datstatic$Diuron_level <- relevel(factor(datstatic$Diuron_level), ref = "low")


# MODEL OF STATIC DATA
#determine stressor interactions for static-static treatments
mod1 <- gam(Proportion_biomass_change ~ ti(Light_conc, k = 3, fx = TRUE) + 
              ti(Diuron_conc, k = 3, fx = TRUE) + 
              ti(Light_conc, Diuron_conc, k = 3, fx = TRUE),
            data = datstatic, method = "REML")
coef(mod1)
plot(mod1)  

par(mfrow = c(2,2))
gam.check(mod1)

summary(mod1)

visreg(mod1, xvar = "Light_conc", 
       by = "Diuron_conc")

visreg(mod1, xvar = "Diuron_conc", 
       by = "Light_conc")

# Predict combined outcomes for H1 
unique(datfluc$Diuron_conc)

#
# Predict to fluctuation data, for H1 (average concentration of stressor, assumes no interaction)
#

datfluc$H1_predictions <- predict(mod1, newdata = datfluc)  
#overall
ggplot(datfluc) + 
  aes(x = H1_predictions, y = Proportion_biomass_change) + 
  geom_point() + 
  facet_wrap(~Stressor_app) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k=3)) +
  geom_abline(slope = 1)
#geom_line(aes(y = Proportion_biomass_change))

## Calculation RMSE as a measure of model 'fit'
datfluc$H1_se <- with(datfluc, {
  (H1_predictions - Proportion_biomass_change)
})


#
# Predict to fluctuation data, for H2 (maximum stressor intensity model, assumes no interaction)
#

#light/diuron maximum stress concentrations
datflucmax <- datfluc %>% select(Light_maxlow, Diuron_maxhigh, Proportion_biomass_change) %>% 
  rename(Light_conc = Light_maxlow, Diuron_conc = Diuron_maxhigh)

datfluc$H2_predictions <- predict(mod1, newdata = datflucmax)  
#overall
ggplot(datfluc) + 
  aes(x = H2_predictions, y = Proportion_biomass_change) + 
  geom_point() + 
  facet_wrap(~Stressor_app) +
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k=3)) +
  geom_abline(slope = 1)
#geom_line(aes(y = Proportion_biomass_change))


## Calculation RMSE as a measure of model 'fit'
datfluc$H2_se <- with(datfluc, {
  (H2_predictions - Proportion_biomass_change)
})


#
# Predict to fluctuation data, for H3 (variable stressor intensity and synchronicity model, identifying stressor interactions)
#

#light/diuron in 1st and 3rd intervals
datfluc13 <- datfluc %>% select(Light_conc1, Diuron_conc1, Proportion_biomass_change) %>% 
  rename(Light_conc = Light_conc1, Diuron_conc = Diuron_conc1)

#light/diuron in 2nd and 4 intervals 
datfluc24 <- datfluc %>% select(Light_conc2, Diuron_conc2, Proportion_biomass_change) %>% 
  rename(Light_conc = Light_conc2, Diuron_conc = Diuron_conc2)

#predictions, simply the average of predictions
#made with dataframes for 1st/3rd and 2nd/4th intervals
datfluc$H3_predictions <- 
  (predict(mod1, newdata = datfluc13) + 
     predict(mod1, newdata = datfluc24))/2

#overall
ggplot(datfluc) + 
  aes(x = H3_predictions, y = Proportion_biomass_change) + 
  geom_point() + 
  facet_wrap(~Stressor_app) + 
  stat_smooth(method = "gam", formula = y ~ s(x, bs = "cs", k=3)) +
  geom_abline(slope = 1)
#geom_line(aes(y = Proportion_biomass_change))

## Calculation RMSE as a measure of model 'fit'
datfluc$H3_se <- with(datfluc, {
  (H3_predictions - Proportion_biomass_change)
})

#determine stressor interactions for fluctuating treatments
#Compare RMSE for each hypothesis, lower is better 
datfluc %>% group_by(Stressor_app) %>%
  summarize(sqrt(mean(H1_se^2)),
            sqrt(mean(H2_se^2)),
            sqrt(mean(H3_se^2)))
View(
  datfluc %>% group_by(Stressor_app, Diuron_conc, Light_conc) %>%
    summarize(H1_rmse = signif(sqrt(mean(H1_se^2)),2),
              H2_rmse = signif(sqrt(mean(H2_se^2)),2),
              H3_rmse = signif(sqrt(mean(H3_se^2)),2),
              H1_bias = signif((mean(H1_se)),2),
              H2_bias = signif((mean(H2_se)),2),
              H3_bias = signif((mean(H3_se)),2)) #%>%
  #mutate(H_Percentage_diff = 100*((H1_rmse - H2_rmse)/H2_rmse))
  #mutate(H2_Percentage_diff = 100*((H1_rmse - H3_rmse)/H1_rmse)),
  #mutate(H3_Percentage_diff = 100*((H2_rmse - H3_rmse)/H2_rmse)) %>%
  #filter(Stressor_app %in% c("InPhase","OutPhase"))
) 
