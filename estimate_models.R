# This script reproduces the linear mixed models of the manuscript "Pupillometry
# is sensitive to speech masking during story listening: The critical role of
# modeling temporal trends" by Andreas Widmann, Björn Herrmann, and Florian
# Scharf.
# 
# Authors: Andreas Widmann, widmann@uni-leipzig.de and Florian Scharf, florian.scharf@uni-kassel.de
# Copyright (c) 2024 Andreas Widmann, Leipzig University and Florian Scharf, University of Kassel

library(lmerTest)
library(emmeans)
library(MuMIn)
library(sjPlot)
library(optimx)

options(scipen = 4, width = 100)
rm(list = ls())

# Load data
load("data.Rdata")

#### SNR x cond ----
m1 <- pa ~ snr_ctr * cond + (1 + cond | subj )
fm1 <- lmer(m1, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm1)
r.squaredGLMM(fm1)
tab_model(fm1, digits = 1)

#### SNR x cond + trial x cond ----
# dat$cond = relevel(dat$cond, ref = "Intact" )
# dat$cond = relevel(dat$cond, ref = "Scrambled" )
m2 <- pa ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2 <- lmer(m2, data = dat,
            control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
            REML = F)
summary(fm2)
r.squaredGLMM(fm2)
tab_model(fm2, digits = 1)
anova(fm1, fm2)

save(file = "models.Rdata", fm1, fm2)
