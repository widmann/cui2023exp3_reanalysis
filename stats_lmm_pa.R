library(lmerTest)
library(MuMIn)
library(psych)
library(ggplot2)
library(emmeans)
library(sjPlot)
library(dplyr)
library(tidyr)
library(optimx)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cuiCol <- c("#4e78f9", "#f73371")

# Import data
dat <- read.csv(file = "eyedata.csv")
dat$subj = factor(dat$subj)
dat$cond = factor(dat$cond, labels = c("Intact", "Scrambled"))
dat$block = factor(dat$block)
# dat$snr_ctr = scale(dat$snr, scale = F, center = T)
dat$snr_ctr = dat$snr - 3
# dat$trial_ctr = scale(dat$trial, scale = F, center = T)
dat$trial_ctr = dat$trial - 11.5

describeBy(pa ~ trial + cond, data = dat, digits = 1, mat = T)

dat_cond_trial <- dat %>%
  group_by(cond, trial) %>%
  summarise(pa_sd = sd(pa), pa_mean = mean(pa))
n <- nlevels(dat$subj)
ggplot(dat_cond_trial, aes(x = trial, y = pa_mean, col = cond)) +
  geom_ribbon(aes(min = pa_mean - pa_sd / sqrt(n) * 1.96, max = pa_mean + pa_sd / sqrt(n) * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggsave("pa_time-on-task.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

# SNR
m1 <- pa ~ snr_ctr * cond + (1 + cond | subj )
fm1 <- lmer(m1, data = dat, control = lmerControl(calc.derivs = F), REML = F)
# fm1 <- lmer(m1, data = dat[dat$trial > 1 & dat$trial < 22,], control = lmerControl(calc.derivs = F), REML = F)
summary(fm1)
r.squaredGLMM(fm1)
tab_model(fm1, digits = 3)

# SNR x cond
# dat$cond = relevel(dat$cond, ref = "Intact" )
# dat$cond = relevel(dat$cond, ref = "Scrambled" )
m2 <- pa ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2 <- lmer(m2, data = dat,
            control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
            REML = F)
summary(fm2)
r.squaredGLMM(fm2)
tab_model(fm2, digits = 3)

emm_df <- emmip(fm2, cond ~ snr_ctr, at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2), plotit = F)
# emm_df <- emmip(fm2, cond ~ snr_ctr, at = list(snr_ctr=-2:2), plotit = F)
emm_df$snr = emm_df$snr_ctr * 5 + 6
ggplot(emm_df, aes(x = snr, y = yvar, col = cond)) +
  # geom_ribbon(aes(min = yvar - SE * 1.96, max = yvar + SE * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggsave("pa_snrxcond.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

# SNR x trial x cond
m3 <- pa ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3 <- lmer(m3, data = dat,
            control = lmerControl(calc.derivs = F, optimizer ='optimx', optCtrl=list(method='nlminb')),
            REML = F)
summary(fm3)
r.squaredGLMM(fm3)

anova(fm2, fm3)
gg <- emmip(fm3, cond ~ snr_ctr | trial_ctr, at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2), col = okabe[c(6,8)])
gg +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_x_continuous(breaks = -2:2, labels = c("-4","1","6","11","16")) +
  labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]", color = "Condition") +
  theme(legend.position = "bottom")
ggsave("pa_condxsnrxtrial-1.pdf", device = "pdf", width = 18 / 2.54, height = 12 / 2.54)

emt <- emtrends(fm3, ~ cond + trial_ctr, var = "snr_ctr", at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2))
emmeans(emt, ~ cond + trial_ctr)
emt_df <- emmip(emt, ~ trial_ctr + cond, plotit = F)

emt_df$trial = emt_df$trial_ctr + 11.5
ggplot(emt_df, aes(x = trial, y = yvar, col = cond)) +
  geom_ribbon(aes(min = yvar - SE * 1.96, max = yvar + SE * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Change in pupil area/+5 dB SNR [a.u.]") +
  theme(legend.position = "bottom")
ggsave("pa_condxsnrxtrial-2.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

# Linked models
m1 <- pa ~ trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond | subj) + (0 + trial_ctr + I(trial_ctr ^ 2) | subj)
fm1 <- lmer(m1, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm1)

dat$pa_corr = dat$pa - predict(fm1)

dat_cond_trial <- dat %>%
  group_by(cond, trial) %>%
  summarise(pa_sd = sd(pa_corr), pa_mean = mean(pa_corr))
n <- length(unique(as.numeric(as.character(dat$subj))))
ggplot(dat_cond_trial, aes(x = trial, y = pa_mean, col = cond)) +
  geom_ribbon(aes(min = pa_mean - pa_sd / sqrt(n) * 1.96, max = pa_mean + pa_sd / sqrt(n) * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")

m2 <- pa_corr ~ snr_ctr * trial_ctr * cond + (1 | subj)
fm2 <- lmer(m2, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm2)
