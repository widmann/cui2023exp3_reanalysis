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
load("bayes_models_effect_coding.Rdata")

# (2) Dann würde ich gerne möglichst analog zu Fig. 9 im Cui-Paper zeigen, wie es aussähe,
#  wenn man es modelliert und Trial als Kovariate ins Modell aufnimmt. 
#  Botschaft: Pupille ist sensitiv für SNR/effort.

#### @ Andreas: Keine Ahnung, wie man die y-Achse sinnvoll skaliert hier, mir fehlt
#### auch dazu die Info, warum die AU bei uns so anders ist als bei denen

#### Panel A: SNR x cond ----

# Wenn man ohne diese Interaktion schätzt bekommt man den sog. marginal effect.
# Das ist sozusagen der mittlere Effekt über alle Trials (vs. der Effekt beim
# mittleren Trial). Das ist ein Unterschied (vermutlich kein starker, aber es
# ist einer), deswegen fragte ich nach. :)

# So (mit trial_ctr = -10.5:10.5) schätzen wir den mittleren Effekt, richtig?

emm_df <- emmip(fm2_bayes, cond ~ snr_ctr, at = list(trial_ctr = -10.5:10.5, snr_ctr = -2:2), plotit = F, CIs = T, PIs = F)
emm_df$snr = emm_df$snr_ctr * 5 + 6
ggplot(emm_df, aes(x = snr, y = yvar, col = cond)) +
  # geom_ribbon(aes(min = yvar - SE, max = yvar + SE, fill = cond), color = NA, alpha = 0.15) +
  geom_ribbon(aes(min = LCL, max = UCL, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(shape = 16, size = 5) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ cond) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggsave("fig2a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

# dat_pred <- dat
# dat_pred$trial_ctr <- 0
# dat_pred$pa_pred = predict(fm2_bayes, dat_pred)
# 
# # Participant center predicted pupil area
# dat_pred$pa_pred.y <- with(dat_pred, ave(pa_pred[, "Estimate"], subj, FUN = mean))
# dat_pred$pa_pred_pCtr <- dat_pred$pa_pred[, "Estimate"] - dat_pred$pa_pred.y
# 
# dat_snr_cond <- dat_pred %>%
#   group_by(subj, snr_ctr, cond) %>%
#   summarise(pa_pred = mean(pa_pred_pCtr)) %>%
#   group_by(snr_ctr, cond) %>%
#   summarise(pa_pred_mean = mean(pa_pred), pa_pred_sd = sd(pa_pred))
# 
# n <- nlevels(dat$subj)
# dat_snr_cond$snr = emm_df$snr_ctr * 5 + 6
# ggplot(dat_snr_cond, aes(x = snr, y = pa_pred_mean, col = cond)) +
#   geom_ribbon(aes(min = pa_pred_mean - pa_pred_sd / sqrt(n) * 1.96, max = pa_pred_mean + pa_pred_sd / sqrt(n) * 1.96, fill = cond), color = NA, alpha = 0.15) +
#   geom_line() +
#   geom_point(shape = 16, size = 5) +
#   scale_fill_manual(values = okabe[c(6,8)]) +
#   scale_color_manual(values = okabe[c(6,8)]) +
#   facet_wrap(~ cond) +
#   labs(color = "Condition", fill = "Condition") +
#   labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
#   theme(legend.position = "bottom")
# ggsave("fig2a_a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

#### Panel B: Slopes

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
ggsave("fig2b.pdf", device = "pdf", width = 4 / 2.54, height = 9 / 2.54)



######## In search of a version without individual differences in the SE

# das hier müsste eine Replikation von deinem sein
conditional_effects(fm2_bayes, effects = "snr_ctr:cond",
                    re_formula = NA) 


fit_cond_lines <- conditional_effects(fm2_bayes, effects = "snr_ctr:cond", 
                    conditions = distinct(dat, subj), 
                    re_formula = NULL)

ind_pred <- fit_cond_lines$`snr_ctr:cond`

new_plot_dat <- aggregate(cbind(estimate__, lower__, upper__, se__) ~ snr_ctr + cond, ind_pred, mean)

ggplot(new_plot_dat, aes(x = snr_ctr, y = estimate__, col = cond)) +
  geom_ribbon(aes(min = lower__, max = upper__, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  facet_wrap(vars(cond)) +
  ylim(1200,1600)


#### andere Idee: rekonstruier die Daten nur aus dem cond-effect, snr-effect
#### und den Residuen



