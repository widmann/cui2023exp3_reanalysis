library(ggplot2)
library(emmeans)
library(dplyr)
library(tidyr)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cuiCol <- c("#4e78f9", "#f73371")

# Import data
load("data.Rdata")

dat_snr_cond <- dat[dat$trial > 1 & dat$trial < 22, ] %>%
  group_by(subj, snr, cond) %>%
  summarise(across(starts_with("pa"), mean)) %>%
  group_by(snr, cond) %>%
  summarise(across(starts_with("pa"), .fns = list(mean = mean, sd = sd)))
n <- nlevels(dat$subj)

dat_snr_cond_long <- pivot_longer(data = dat_snr_cond, cols = !c(snr, cond), 
                                names_to = c("dv", ".value"), names_pattern = "(.+)_(.+)")

dat_snr_cond_long <- rbind(dat_snr_cond_long, dat_snr_cond_long)
dat_snr_cond_long$ci <- factor(rep(c(0,1), each = 20), labels = c("SE", "CI"))
dat_snr_cond_long$sd <- dat_snr_cond_long$sd / sqrt(n)
dat_snr_cond_long[dat_snr_cond_long$ci == "CI",]$sd <- dat_snr_cond_long[dat_snr_cond_long$ci == "CI",]$sd * 1.96
dat_snr_cond_long <- dat_snr_cond_long[dat_snr_cond_long$dv != "pa.y",]

ggplot(dat_snr_cond_long, aes(x = snr, y = mean, col = cond)) +
  # geom_ribbon(aes(min = pa_mean - pa_sd / sqrt(n) * 1.96, max = pa_mean + pa_sd / sqrt(n) * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_ribbon(aes(min = mean - sd, max = mean + sd, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  facet_grid(dv ~ ci, scales = "free")
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggsave("fig_sd.pdf", device = "pdf", width = 18 / 2.54, height = 12 / 2.54)

# (2) Dann würde ich gerne möglichst analog zu Fig. 9 im Cui-Paper zeigen, wie es aussähe,
#  wenn man es modelliert und Trial als Kovariate ins Modell aufnimmt. 
#  Botschaft: Pupille ist sensitiv für SNR/effort.

#### @ Andreas: Keine Ahnung, wie man die y-Achse sinnvoll skaliert hier, mir fehlt
#### auch dazu die Info, warum die AU bei uns so anders ist als bei denen

load("models.Rdata")

#### Panel A: SNR x cond

# Wenn man ohne diese Interaktion schätzt bekommt man den sog. marginal effect.
# Das ist sozusagen der mittlere Effekt über alle Trials (vs. der Effekt beim
# mittleren Trial). Das ist ein Unterschied (vermutlich kein starker, aber es
# ist einer), deswegen fragte ich nach. :)

# So (mit trial_ctr = -10.5:10.5) schätzen wir den mittleren Effekt, richtig?

emm_df <- emmip(fm2, cond ~ snr_ctr, at = list(trial_ctr = -10.5:10.5, snr_ctr = -2:2), plotit = F, CIs = T)
emm_df$snr = emm_df$snr_ctr * 5 + 6
ggplot(emm_df, aes(x = snr, y = yvar, col = cond)) +
  geom_ribbon(aes(min = LCL, max = UCL, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(shape = 16, size = 5) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggsave("fig2a_ctr.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

# Pseudo-randomize trials 1 and 22 for intact and scrambled conditions
# Constraints:
#   * 4 different SNRs per participant
#   * Equal number of SNRs per condition (approximately as N = 23)
set.seed(999)
set_intact <- sample(1:5, 5)
for(irun in 1:9) {
  set_intact <- append(set_intact, sample(setdiff(1:5, set_intact[length(set_intact)]), 1))
  set_intact <- append(set_intact, sample(setdiff(1:5, set_intact[length(set_intact)]), 4))
}
set_intact <- matrix(set_intact, ncol = 2, byrow = T)
set_intact
# set_scrambled <- sample(1:5, 5)
# for(irun in 1:9) {
#   set_scrambled <- append(set_scrambled, sample(setdiff(1:5, set_scrambled[length(set_scrambled)]), 1))
#   set_scrambled <- append(set_scrambled, sample(setdiff(1:5, set_scrambled[length(set_scrambled)]), 4))
# }
# set_scrambled <- matrix(set_scrambled, ncol = 2, byrow = T)

set_scrambled <- matrix(nrow = 25, ncol = 2)
for(irow in seq(3, 25, 5)) {
  set_scrambled[irow, ] <- sample(setdiff(1:5, set_intact[irow, ]), 2)
  set_scrambled[irow - 1, ] <- sample(setdiff(1:5, append(set_scrambled[irow, 1], set_intact[irow - 1, ])), 2)
  set_scrambled[irow - 2, ] <- sample(setdiff(1:5, append(set_scrambled[irow, 1], set_scrambled[irow - 1, ])), 2)
  set_scrambled[irow + 1, ] <- sample(setdiff(1:5, append(set_scrambled[irow, 2], set_intact[irow + 1, ])), 2)
  set_scrambled[irow + 2, ] <- sample(setdiff(1:5, append(set_scrambled[irow, 2], set_scrambled[irow + 1, ])), 2)
}

dat_pred <- data.frame(cbind(subj = rep(1:23, each = 220), cond = rep(1:2, each = 110), snr_ctr = rep(-2:2, 2 * 23, each = 22), trial_ctr = rep(seq(-10.5, 10.5, 1), 2 * 5 * 23)))
dat_pred$subj <- factor(dat_pred$subj)
dat_pred$cond <- factor(dat_pred$cond, labels = c("Intact", "Scrambled"))

dat_pred <- dat
dat_pred[dat_pred$trial == 1 & dat_pred$cond == "Intact",]$snr <- set_intact[1:23, 1]
dat_pred[dat_pred$trial == 22 & dat_pred$cond == "Intact",]$snr <- set_intact[1:23, 2]
dat_pred[dat_pred$trial == 1 & dat_pred$cond == "Scrambled",]$snr <- set_scrambled[1:23, 1]
dat_pred[dat_pred$trial == 22 & dat_pred$cond == "Scrambled",]$snr <- set_scrambled[1:23, 2]
dat_pred$snr_ctr <- dat_pred$snr - 3


dat_pred <- dat
dat_pred$trial_ctr <- 0
# dat_pred[dat_pred$trial == 1 | dat_pred$trial == 22, ]$snr_ctr <- 0
# dat_pred <- dat_pred[dat_pred$trial > 1 & dat_pred$trial < 22, ]
dat_pred$pa_pred = predict(fm3_bayes, dat_pred)

# Participant center predicted pupil area
dat_pred$pa_pred.y <- with(dat_pred, ave(pa_pred[,"Estimate"], subj, FUN = mean))
dat_pred$pa_pred_pCtr <- dat_pred$pa_pred[,"Estimate"] - dat_pred$pa_pred.y

dat_snr_cond <- dat_pred %>%
  group_by(subj, snr_ctr, cond) %>%
  # summarise(pa_pred = mean(pa_pred[,"Estimate"])) %>%
  summarise(pa_pred = mean(pa_pred_pCtr)) %>%
  group_by(snr_ctr, cond) %>%
  summarise(pa_pred_mean = mean(pa_pred), pa_pred_sd = sd(pa_pred))

n <- nlevels(dat$subj)
ggplot(dat_snr_cond, aes(x = snr_ctr, y = pa_pred_mean, col = cond)) +
  geom_ribbon(aes(min = pa_pred_mean - pa_pred_sd / sqrt(n) * 1.96, max = pa_pred_mean + pa_pred_sd / sqrt(n) * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggplot(emm_df, aes(x = snr_ctr, y = yvar, col = cond)) +
  geom_ribbon(aes(min = LCL, max = UCL, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point() +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")

#### Panel B: Slopes

library(posterior)

# in deren Grafik sind ja dann noch die individuellen Effekte, damit befassen wir uns nun...

### individuelle Effekte rausholen
ind_slopes <- data.frame(subj = unique(dat$subj),
  slopes_intact = coef(fm2a_bayes)$subj[,"Estimate", "snr_ctr"],
  slopes_scrambled = rowSums(coef(fm2a_bayes)$subj[,"Estimate", c("snr_ctr","snr_ctr:condScrambled")]))

ind_slopes_long <- pivot_longer(data = ind_slopes, cols = !c(subj), 
                                   names_to = "cond", values_to = "slopes")

ind_slopes_long$cond <- factor(ind_slopes_long$cond, labels = levels(dat$cond))

### mittlere Effekte umrechnen 
# extrahiere posteriori Werte der Koeffizienten
posterior_samples <- as_draws(fm2a_bayes, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
posterior_samples <- merge_chains(posterior_samples, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
posterior_samples <- as_draws_df(posterior_samples)

# Man kann aus der Posterior die KIs bestimmen, erstmal als Demo/Check
fixef(fm2a_bayes)["snr_ctr",]
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

ggplot(ind_slopes_long, aes(x = cond, y = slopes, group = subj, fill = cond, col = cond)) +
  geom_point(shape = 16, alpha = 0.4, size = 2) +
  geom_line(alpha = 0.4, col = "gray") +
  geom_point(shape = 16, data = avg_slopes, aes(group = NULL), size = 5) +
  geom_errorbar(data = avg_slopes, aes(group = NULL, min = CIs.2.5., max = CIs.97.5.), width = 0.1) +
  facet_grid(~ "Slope") +
  scale_color_manual(values = okabe[c(6,8)]) +
  labs(x = "Condition", y = "Change in pupil area/+5 dB SNR [a.u.]") +
  theme(legend.position = "bottom") 
ggsave("fig2b_ctr.pdf", device = "pdf", width = 4 / 2.54, height = 9 / 2.54)

