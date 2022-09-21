#Table S3 (supplementary materials)
#Determine coefficient estimates of static-static model to interpret stressor interactions
library(mgcv)
library(readr)
library(readxl)
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