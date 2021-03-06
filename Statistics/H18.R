rm(list=ls())
a <- read.table("ObserverRepeatability.txt", header=T)
a
require(dplyr)
a %>% group_by(StudentID) %>% summarise(count=length(StudentID)) %>% summarise(length(StudentID))
a %>% group_by(StudentID) %>% summarise(count=length(StudentID)) %>% summarise(sum(count^2))
mod <- lm(Tarsus~StudentID, data=a)
anova(mod)
mod2 <- lm(Tarsus~Leg+Handedness+StudentID, data=a)
anova(mod2)
require(lme4)
lmm <- lmer(Tarsus~Leg+Handedness+(1|StudentID), data=a)
summary(lmm)


