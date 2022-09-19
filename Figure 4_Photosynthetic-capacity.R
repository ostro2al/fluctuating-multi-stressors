#Figure 4 - Analysis of experimental results, model predictions
#Using lmer to model predictions for change in photosynthetic capacity
library(lmerTest)
library(lme4)
library(readr)
library(ggplot2)
library(tidyverse)

PE_avg <- read.csv("PE_days_avg.csv", fileEncoding = 'UTF-8-BOM')
PE_avg <- as.data.frame(unclass(PE_avg), stringsAsFactors = TRUE)
PE_avg$Week <- as.factor(PE_avg$Week)
hist(PE_avg$Avg_Photo_Total)
str(PE_avg)
PE_avg$Stressor_app <- relevel(factor(PE_avg$Stressor_app), 
                               ref = "StaticStatic")

#transform data
PE_avg$Avg_Photo_Total2 <- PE_avg$Avg_Photo_Total^2
hist(PE_avg$Avg_Photo_Total2)
PE_avg$Day0PE2 <- PE_avg$Day0PE^2
hist(PE_avg$Day0PE2)

#random effect of plant ID
m1 <- lmer(Avg_Photo_Total2 ~ Stressor_app * Light_level * Diuron_level * Day +
             (1|ID) + offset(Day0PE2), data = PE_avg)
m2 <- lmer(Avg_Photo_Total2 ~ Stressor_app*Light_level*Diuron_level + Day +
             (1|ID) + offset(Day0PE2), data = PE_avg)
m3 <- lmer(Avg_Photo_Total2 ~ Stressor_app + Light_level*Diuron_level*Day + 
             (1|ID) + offset(Day0PE2), data = PE_avg)
m4 <- lmer(Avg_Photo_Total2 ~ Stressor_app*Day + Light_level*Diuron_level +
             (1|ID) + offset(Day0PE2), data = PE_avg)
m5 <- lmer(Avg_Photo_Total2 ~ Stressor_app*Day + Light_level + Diuron_level +
             (1|ID) + offset(Day0PE2), data = PE_avg)
m6 <- lmer(Avg_Photo_Total2 ~ Stressor_app +  Day + Light_level*Diuron_level +
             (1|ID) + offset(Day0PE2), data = PE_avg)

a <- AIC(m1, m2, m3, m4, m5, m6)
a

#m6 has lowest AIC
plot(m6)
qqnorm(resid(m6))
qqline(resid(m6))
summary(m6)
anova(m6)

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
  select(Stressor_app, Light_level, Diuron_level, Day) %>%
  distinct()
newdata$Day0PE2 <- mean(PE_avg$Day0PE2)
newdata$mean <- predict(m6, re.form=NA,
                        newdata = newdata)

dat$Light_level <- factor(dat$Light_level, levels = c("Medium Light", "Low Light"))
dat$Diuron_level <- factor(dat$Diuron_level, levels = c("Medium Herbicide", "High Herbicide"))
dat$Stressor_app <- factor(dat$Stressor_app, levels = c("StaticStatic", "StaticFlux", "FluxStatic", "InPhase", "OutPhase"))

SEs <- easyPredSE(m6, newdata = newdata)
newdata$lwr <- SEs[,1]
newdata$upr <- SEs[,2]

#predictions plot filtered for final day with SE
newdata %>%
  filter(Day == 7) %>%
  ggplot() +
  aes(x = Light_level, y = mean-Day0PE2, colour = Stressor_app) +
  ylim(-0.4, -0.2) +
  geom_errorbar(aes(ymin = lwr-Day0PE2, ymax = upr-Day0PE2), width = 0.2, position = position_dodge(width = 0.6)) +
  geom_point(size = 4,  position = position_dodge(0.6)) +
  #geom_linerange(aes(ymin = lwr-Day0PE2, ymax = upr-Day0PE2) +
  geom_line(aes(y = mean-Day0PE2), size = 1) +
  facet_grid(~Diuron_level) +
  theme_bw()+
  theme(text = element_text(family="Times New Roman", size = 12, face = "bold"), 
        axis.text.x = element_text(angle = 0, vjust=0.6, color = "black"),
        axis.text.y = element_text(color = "black"))+
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank())+
  geom_hline(yintercept = 0, colour = "grey42", size = 0.5, linetype = "longdash")+
  ylab("Model predictions (photosynthetic capacity \n change relative to day 0)") +
  xlab("Light Level") + 
  scale_colour_discrete("Stressor application")
