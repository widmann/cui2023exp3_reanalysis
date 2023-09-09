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

# in deren Grafik sind ja dann noch die individuellen Effekte, damit befassen wir uns nun...

# die individuellen SNR-Trends kriegt man nur in brms
# weil wir dafür das Modell mit dem snr-Slope schäten müssen
library(brms)
library(posterior)
# Modell von oben replizieren, Koeffizienten etc. passen
# m2 <- pa ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond + trial_ctr + I(trial_ctr ^ 2) | subj)
# fm2_bayes <- brm(m2, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))
# summary(fm2_bayes)
# plot(fm2_bayes)
# pp_check(fm2_bayes, ndraws = 100)

# nun das Modell mit dem snr und der condition Interaktion, so bekommt
# jede Person einen individuellen SNR-Slope in jeder Bedingung
m3 <- pa ~ snr_ctr * trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_bayes <- brm(m3, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))
summary(fm3_bayes)
# plot(fm3_bayes)
pp_check(fm3_bayes, ndraws = 100)

# ranef(fm3_bayes)$subj[,"Estimate",]

### individuelle Effekte rausholen
ind_slopes <- data.frame(subj = unique(dat_vp$subj),
  slopes_intact = coef(fm3_bayes)$subj[,"Estimate", "snr_ctr"],
  slopes_scrambled = rowSums(coef(fm3_bayes)$subj[,"Estimate", c("snr_ctr","snr_ctr:condScrambled")]))

ind_slopes_long <- pivot_longer(data = ind_slopes, cols = !c(subj), 
                                   names_to = "cond", values_to = "slopes")

ind_slopes_long$cond <- factor(ind_slopes_long$cond, labels = levels(dat$cond))

### mittlere Effekte umrechnen 
# extrahiere posteriori Werte der Koeffizienten
posterior_samples <- as_draws(fm3_bayes, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
posterior_samples <- merge_chains(posterior_samples, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
posterior_samples <- as_draws_df(posterior_samples)

# Man kann aus der Posterior die KIs bestimmen, erstmal als Demo/Check
fixef(fm3_bayes)["snr_ctr",]
quantile(posterior_samples$b_snr_ctr, probs = c(0.025, 0.975))
# für den Effekt in der anderen Bedingung, muss man die Koeffizienten addieren
posterior_samples$b_snr_ctr_scrambled <- posterior_samples$b_snr_ctr + posterior_samples$`b_snr_ctr:condScrambled`

# berechne die mittleren slopes der beiden Bedingungen und generiere die KIs dafür
# aus den posteriori Quantilen
CIs <- apply(posterior_samples[,c("b_snr_ctr","b_snr_ctr_scrambled")], 2, quantile, probs = c(0.025, 0.975))
CIs

avg_slopes <- data.frame(cond = levels(dat$cond), 
                         slopes = colMeans(posterior_samples[,c("b_snr_ctr","b_snr_ctr_scrambled")]),
                         CIs = t(CIs))

ggplot(ind_slopes_long, aes(x = cond, y = slopes, group = subj, col = cond)) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.3, col = "gray") +
  geom_point(data = avg_slopes, aes(group = NULL), size = 5) +
  geom_errorbar(data = avg_slopes, aes(group = NULL, min = CIs.2.5., max = CIs.97.5.)) +
  scale_color_manual(values = okabe[c(6,8)]) +
  theme(legend.position = "bottom") 

#### @ Andreas: eine überdenkenswerte Sache hier wäre: 
#### im Moment zeige ich ja den Slope des SNR im mittleren Trial, ist das gut?



# (3) Und dann würde ich gerne zugänglich visualisieren, wie sich der Effekt im Zeitverlauf entwickelt,
#  wenn man die Interaktion zulässt. Botschaft: Pupille ist sogar so sensitiv, 
#  dass sich die Veränderung des Effektes im Verlauf des Experimentes auswerten lässt.

# aus dem polynomialen Modell heraus:
summary(fm3_bayes)
conditions <- make_conditions(fm3_bayes, vars = c("trial_ctr"))

my_plot <- conditional_effects(fm3_bayes, effects = "snr_ctr:cond", 
                    conditions = conditions,
                    # re_formula = NULL)
                    re_formula = NA)
plot(my_plot) 


## aus einem smoothing spline heraus (ohne Annahme bestimmter Verlaufsformen)

m4 <- pa ~ s(snr_ctr,trial_ctr, by = cond) + s(trial_ctr, subj, bs = 'fs', m=1) + (1 + cond| subj)
# m4 <- pa ~ s(snr_ctr,trial_ctr, by = cond) + (1 + cond| subj)
fm4_bayes <- brm(m4, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))
summary(fm4_bayes)
# plot(fm4_bayes) 
pp_check(fm4_bayes, ndraws = 100)


plot(conditional_smooths(fm4_bayes, 
                         rug = TRUE, 
                         ask = FALSE, 
                         spaghetti = TRUE,
                         nt_conditions = list(trial_ctr = quantile)), stype = "raster")


conditions <- make_conditions(fm4_bayes, vars = c("trial_ctr"))
conditional_effects(fm4_bayes, effects = "snr_ctr:cond", 
                    conditions = conditions,
                    # re_formula = NULL)
                    re_formula = NA)

save(file = "allFits.Rdata", list = ls())
