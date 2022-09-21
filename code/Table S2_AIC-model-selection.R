#Table S2 (supplementary materials)
#model selection using AIC

#Using lmer to model predictions for change in photosynthetic capacity
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
