#Figure 3 - Analysis of experimental results, model predictions
#using lmer to model predictions for biomass proportional change
library(lmerTest)
library(lme4)
library(readr)
library(ggplot2)
library(tidyverse)

dat <- read.csv("Biomass_change.csv", fileEncoding = 'UTF-8-BOM')
dat <- as.data.frame(unclass(dat), stringsAsFactors = TRUE)
dat$Week <- as.factor(dat$Week)
hist(dat$Biomass)
str(dat)
dat$Stressor_app <- relevel(factor(dat$Stressor_app), 
                            ref = "StaticStatic")  


m1 <- lmer(Biomass ~ Stressor_app * Light_level * Diuron_level +
             (1|Week), data = dat)

plot(m1)
qqnorm(resid(m1))
qqline(resid(m1))
summary(m1)
anova(m1)

#formatted figure with SE
easyPredSE <- function(model,newdata=NULL,alpha=0.05) {
  ## baseline prediction, on the linear predictor (logit) scale:
  pred0 <- predict(model,re.form=NA,newdata=newdata)
  ## fixed-effects model matrix for new data
  X <- model.matrix(formula(model,fixed.only=TRUE)[-2],newdata)
  beta <- fixef(model) ## fixed-effects coefficients
  V <- vcov(model)     ## variance-covariance matrix of beta
  pred.se <- sqrt(diag(X %*% V %*% t(X))) ## std errors of predictions
  ## inverse-link function
  linkinv <- family(model)$linkinv
  crit <- -qnorm(alpha/2)
  linkinv(cbind(conf.low=pred0-pred.se,
                conf.high=pred0+pred.se))
}

newdata <- dat %>% 
  select(Stressor_app, Light_level, Diuron_level) %>%
  distinct()
newdata$mean <- predict(m1, re.form=NA,
                        newdata = newdata)

dat$Light_level <- factor(dat$Light_level, levels = c("Medium Light", "Low Light"))
dat$Diuron_level <- factor(dat$Diuron_level, levels = c("Medium Herbicide", "High Herbicide"))
dat$Stressor_app <- factor(dat$Stressor_app, levels = c("StaticStatic", "StaticFlux", "FluxStatic", "InPhase", "OutPhase"))

SEs <- easyPredSE(m1, newdata = newdata)
newdata$lwr <- SEs[,1]
newdata$upr <- SEs[,2]

newdata %>%
  ggplot() +
  aes(x = Light_level, y = mean, colour = Stressor_app) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_point(size = 4,  position = position_dodge(0.5)) +
  #geom_linerange(aes(ymin = lwr, ymax = upr) +
  geom_line(aes(y = mean), size = 1) +
  facet_grid(~Diuron_level) +
  theme_bw()+
  theme(text = element_text(family="Times New Roman", size = 12, face = "bold"), 
        axis.text.x = element_text(angle = 0, vjust=0.6, color = "black"),
        axis.text.y = element_text(color = "black"))+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  geom_hline(yintercept = 0, colour = "grey42", size = 0.5, linetype = "longdash")+
  ylab("Model predictions \n (proportional biomass change)") +
  xlab("Light Level") + 
  scale_colour_discrete("Stressor application")
