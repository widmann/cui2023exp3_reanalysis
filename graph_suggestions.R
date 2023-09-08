library(lmerTest)
library(MuMIn)
library(psych)
library(ggplot2)
library(emmeans)
library(sjPlot)
library(dplyr)
library(tidyr)

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

m2 <- pa ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2 <- lmer(m2, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm2)



# (1) Ich würde unbedingt gerne die Pupille im Zeitverlauf zeigen. 
# Botschaft: Das ist die Ursache des Übels. 
# Trial 1 mit fixen +16 dB führt einen massiven Bias ein 
# und das „Rauschen“ ist um Größenordnungen größer als der Effekt.

# Erstelle einen reduzierten Datensatz mit allen Prädiktorkombinationen
dat_unique <- unique(dat[, c("snr_ctr", "trial_ctr", "cond")])
# Erstelle dazu die Dummies usw.
Xmat <- model.matrix(~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond, data = dat_unique)
# individuelle Koeffiezienten extrahieren
indiv_coefs <- coef(fm2)$subj
# individuelle Modellvorhersagen berechnen
ind_predictions <- Xmat %*% t(indiv_coefs)
dat_predicted <- data.frame(dat_unique, ind_predictions)


dat_predicted_long <- pivot_longer(data = dat_predicted, cols = !c("snr_ctr", "trial_ctr", "cond"), 
             names_to = "subj", values_to = "pa")
dat_predicted_long$snr_ctr <- factor(dat_predicted_long$snr_ctr)

# Aus dem Modell heraus plotten:
emm_df <- emmip(fm2, cond ~ trial_ctr * snr_ctr, at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2), plotit = F)
emm_df$snr_ctr <- as.factor(emm_df$snr_ctr)
ggplot(emm_df, aes(x = trial_ctr, y = yvar, col = cond, shape = snr_ctr)) +
  # geom_ribbon(aes(min = yvar - SE * 1.96, max = yvar + SE * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom") +
  geom_point(data = dat_predicted_long, aes(x = trial_ctr, y = pa, col = cond, shape = snr_ctr, group = subj), alpha = 0.3) +
  geom_line(data = dat_predicted_long, aes(x = trial_ctr, y = pa, col = cond, group = interaction(subj, snr_ctr)), alpha = 0.3) 
  
  
  
### OHNE Trennung nach SNR (gemittelt über alle SNRs als marginal effect)

m_reduced <- pa ~ trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm_reduced <- lmer(m_reduced, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm_reduced)


# Erstelle einen reduzierten Datensatz mit allen Prädiktorkombinationen
dat_unique <- unique(dat[, c("trial_ctr", "cond")])
# Erstelle dazu die Dummies usw.
Xmat <- model.matrix(~ trial_ctr * cond + I(trial_ctr ^ 2) * cond, data = dat_unique)
# individuelle Koeffiezienten extrahieren
indiv_coefs <- coef(fm_reduced)$subj
# individuelle Modellvorhersagen berechnen
ind_predictions <- Xmat %*% t(indiv_coefs)
dat_predicted <- data.frame(dat_unique, ind_predictions)


dat_predicted_long <- pivot_longer(data = dat_predicted, cols = !c("trial_ctr", "cond"), 
                                   names_to = "subj", values_to = "pa")

emm_df <- emmip(fm_reduced, cond ~ trial_ctr, at = list(trial_ctr=-10.5:10.5), plotit = F)
ggplot(emm_df, aes(x = trial_ctr, y = yvar, col = cond)) +
  geom_ribbon(aes(min = yvar - SE * 1.96, max = yvar + SE * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom") +
  geom_point(data = dat_predicted_long, aes(x = trial_ctr, y = pa, col = cond, group = subj), alpha = 0.3) +
  geom_line(data = dat_predicted_long, aes(x = trial_ctr, y = pa, col = cond, group = subj), alpha = 0.3) 




### oder direkt aus den Rohdaten zum Vergleich
dat_vp <- aggregate(pa ~ subj + trial + cond, data = dat, FUN = mean) # könnte man auch direkt aus den Daten ziehen, ich wollte nur "sichergehen"
dat_mean <- aggregate(pa ~ trial + cond, data = dat, FUN = function(x) {c(M = mean(x), SD = sd(x), Q = quantile(x, probs = c(0.05, 0.95)))})
dat_mean <- data.frame(dat_mean[,1:2], dat_mean$pa)

ggplot(dat_vp, aes(x = trial, y = pa, col = cond, group = subj)) +
  geom_line(alpha = 0.3) +
  geom_point(alpha = 0.3) +
  geom_point(data = dat_mean, aes(x = trial, y = M, col = cond, group = NULL), size = 3) +
  geom_line(data = dat_mean, aes(x = trial, y = M, col = cond, group = NULL), linewidth = 2) +
  # ich hab als Ribbons mal jeweils das 5% und 95% Quantil der Daten angesetzt 
  geom_ribbon(data = dat_mean, aes(y = NULL , min = Q.5., max = Q.95., fill = cond, group = NULL), color = NA, alpha = 0.15) +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial", y = "Measured pupil area [a.u.]") +
  theme(legend.position = "bottom")

# ODER alles als smooths, sieht dann halt nicht so rauschig aus
ggplot(dat_vp, aes(x = trial, y = pa, col = cond, group = subj)) +
  geom_smooth(data = dat_mean, aes(x = trial, y = M, col = cond, group = NULL), linewidth = 2) +
  geom_smooth(linewidth = 0.5, se = F) +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial", y = "Measured pupil area [a.u.]") +
  theme(legend.position = "bottom")

# (2) Dann würde ich gerne möglichst analog zu Fig. 9 im Cui-Paper zeigen, wie es aussähe,
#  wenn man es modelliert und Trial als Kovariate ins Modell aufnimmt. 
#  Botschaft: Pupille ist sensitiv für SNR/effort.


#### @ Andreas: Keine Ahnung, wie man die y-Achse sinnvoll skaliert hier, mir fehlt
#### auch dazu die Info, warum die AU bei uns so anders ist als bei denen


emm_df <- emmip(fm2, cond ~ trial_ctr * snr_ctr, at = list(snr_ctr=-2:2), plotit = F)
ggplot(emm_df, aes(x = snr_ctr, y = yvar, col = cond)) +
  geom_errorbar(aes(min = yvar - SE, max = yvar + SE)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")



# alternative: SNR als Faktor auffassen
m2_disc <- pa ~ factor(snr_ctr) * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2_disc <- lmer(m2_disc, data = dat, control = lmerControl(calc.derivs = F), REML = F)
summary(fm2_disc)


# dann ist es natürlich nicht so geglättet, aber man erzwingt auch keine Linearität
emm_df <- emmip(fm2_disc, cond ~ trial_ctr * factor(snr_ctr), plotit = F)
ggplot(emm_df, aes(x = snr_ctr, y = yvar, col = cond)) +
  geom_errorbar(aes(min = yvar - SE, max = yvar + SE)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")




# (3) Und dann würde ich gerne zugänglich visualisieren, wie sich der Effekt im Zeitverlauf entwickelt,
#  wenn man die Interaktion zulässt. Botschaft: Pupille ist sogar so sensitiv, 
#  dass sich die Veränderung des Effektes im Verlauf des Experimentes auswerten lässt.

### TO-DO FS ###
### Individual REs für snr für (2) als model implied version
### CONDOTIONAL EFFEKT PLOTTEN für (3)

