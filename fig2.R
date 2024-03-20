# This script reproduces Fig. 2 of the manuscript "Pupillometry is sensitive to
# speech masking during story listening: The critical role of modeling temporal
# trends" by Andreas Widmann, Björn Herrmann, and Florian Scharf.
#
# Authors: Florian Scharf, florian.scharf@uni-kassel.de and Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Florian Scharf, University of Kassel and Andreas Widmann, Leipzig University

library(ggplot2)
library(emmeans)
library(dplyr)
library(tidyr)
library(brms)
library(posterior)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cuiCol <- c("#4e78f9", "#f73371")

# Import data
load("data.Rdata")
load("bayes_models.Rdata")

#### Panel A: SNR x cond, CI version ----

emm_df <- emmip(fm2_bayes, cond ~ snr_ctr, at = list(trial_ctr = -10.5:10.5, snr_ctr = -2:2), plotit = F, CIs = T, PIs = F)
emm_df$snr <- emm_df$snr_ctr * 5 + 6
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
# ggsave("fig2a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

#### Panel A: SNR x cond, within SE version ----

######## In search of a version without individual differences in the SE

# Idee: berechne die individuellen Grafiken für alle Personen und Trials einzeln und mittle die, 
# so dass man nur noch die within-person 
# Unsicherheit hat.

### Idee: splitte die Arbeit in Chunks, berechnet die Werte pro Chunk und dann fügen wir alles wieder zusammen
# das dauert SEHR LANGE

# all_fits <- lapply(split(levels(dat$subj), ceiling(seq_along(levels(dat$subj))/3)), FUN = function(iBatch){
#   tmp <- conditional_effects(fm2_bayes, effects = "snr_ctr:cond",
#                              conditions = distinct(dat[dat$subj %in% iBatch,], subj, trial_ctr),
#                              re_formula = NULL,
#                              robust = FALSE,
#                              plot = F)
#   return(tmp)
# }
# )
# 
# # save(list = ls(), file = "ind_fits.Rdata")
# 
# all_fits2 <- lapply(all_fits, function(iFit){
#   iFit$`snr_ctr:cond`
# })
# 
# ind_pred <- bind_rows(all_fits2, .id = "column_label")
# save(list = "ind_pred", file = "ind_fits_from_f2.Rdata")

load(file = "ind_fits_from_f2.Rdata")
new_plot_dat <- aggregate(cbind(estimate__, lower__, upper__, se__) ~ snr_ctr + cond, ind_pred, mean)
new_plot_dat$snr = new_plot_dat$snr_ctr * 5 + 6

ggplot(new_plot_dat, aes(x = snr, y = estimate__, col = cond)) +
  geom_ribbon(aes(min = estimate__ - se__, max = estimate__ + se__, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(data = new_plot_dat[round((new_plot_dat$snr_ctr + 2) * 99 / 100 * 25 + 1) %in% c(1, 26, 50, 75, 100),], aes(x = snr, y = estimate__, col = cond), shape = 16, size = 5) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")
# ggsave("fig2a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

#### Panel B: Slopes ----

# in deren Grafik sind ja dann noch die individuellen Effekte, damit befassen wir uns nun...

### individuelle Effekte rausholen
ind_slopes <- data.frame(subj = unique(dat$subj),
  slopes_intact = coef(fm2_bayes)$subj[,"Estimate", "snr_ctr"],
  slopes_scrambled = rowSums(coef(fm2_bayes)$subj[,"Estimate", c("snr_ctr","snr_ctr:condScrambled")]))

ind_slopes_long <- pivot_longer(data = ind_slopes, cols = !c(subj), 
                                   names_to = "cond", values_to = "slopes")

ind_slopes_long$cond <- factor(ind_slopes_long$cond, labels = levels(dat$cond))

### mittlere Effekte umrechnen 
# extrahiere posteriori Werte der Koeffizienten
posterior_samples <- as_draws(fm2_bayes, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
posterior_samples <- merge_chains(posterior_samples, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
posterior_samples <- as_draws_df(posterior_samples)

# Man kann aus der Posterior die KIs bestimmen, erstmal als Demo/Check
fixef(fm2_bayes)["snr_ctr",]
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
  geom_errorbar(data = avg_slopes, aes(group = NULL, min = CIs.2.5., max = CIs.97.5.), width = 0.25) +
  facet_grid(~ "SNR linear term") +
  scale_color_manual(values = okabe[c(6,8)]) +
  labs(x = "Condition", y = "Change in pupil area/+5 dB SNR [a.u.]") +
  theme(legend.position = "bottom") 
# ggsave("fig2b.pdf", device = "pdf", width = 4 / 2.54, height = 9 / 2.54)




