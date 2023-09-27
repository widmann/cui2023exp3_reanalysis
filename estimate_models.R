library(lmerTest)
library(emmeans)
library(MuMIn)
library(sjPlot)
library(optimx)

options(scipen = 4, width = 100)
rm(list = ls())

# Import data
load("data.Rdata")

#### SNR x cond ----
m1 <- pa ~ snr_ctr * cond + (1 + cond | subj )
fm1 <- lmer(m1, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm1)
r.squaredGLMM(fm1)
tab_model(fm1, digits = 3)

#### SNR x cond + trial x cond ----
# dat$cond = relevel(dat$cond, ref = "Intact" )
# dat$cond = relevel(dat$cond, ref = "Scrambled" )
m2 <- pa ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2 <- lmer(m2, data = dat,
            control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
            REML = F)
summary(fm2)
r.squaredGLMM(fm2)
tab_model(fm2, digits = 3)

#### SNR x trial x cond ----
m3 <- pa ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3 <- lmer(m3, data = dat,
            control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
            REML = F)
summary(fm3)
r.squaredGLMM(fm3)
tab_model(fm3, digits = 3)

anova(fm2, fm3)

#### fixation duration: SNR x trial x cond ----
m3_fd <- fd ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_fd <- lmer(m3_fd, data = dat,
            control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
            REML = F)
summary(fm3_fd)
r.squaredGLMM(fm3_fd)
tab_model(fm3_fd, digits = 5)

emt <- emtrends(fm3_fd, ~ cond + trial_ctr, var = "snr_ctr", at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2))
emmeans(emt, ~ cond + trial_ctr)

#### spatial gaze dispersion: SNR x trial x cond ----
m3_sgdi <- sgd_i ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_sgdi <- lmer(m3_sgdi, data = dat,
               control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
               REML = F)
summary(fm3_sgdi)
r.squaredGLMM(fm3_sgdi)
tab_model(fm3_sgdi, digits = 5)

emt <- emtrends(fm3_sgdi, ~ cond + trial_ctr, var = "snr_ctr", at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2))
emmeans(emt, ~ cond + trial_ctr)

save(file = "models.Rdata", fm1, fm2, fm3, fm3_fd, fm3_sgdi)
