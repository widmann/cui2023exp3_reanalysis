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
load(file = "data.Rdata")

#### Panel A: Grand-average time course

dat_cond_trial <- dat %>%
  group_by(cond, trial) %>%
  summarise(pa_sd = sd(pa_ctr), pa_mean = mean(pa_ctr))
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
ggsave("fig1a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

#### Panel B: Linear trend

load("bayes_models.Rdata")

ranef(fm2a_bayes)$subj[,"Estimate",]
fixef(fm2a_bayes)

### individuelle Effekte rausholen
ind_slopes <- data.frame(subj = unique(dat$subj),
                         slopes_intact = coef(fm_reduced)$subj[,"trial_ctr"],
                         slopes_scrambled = rowSums(coef(fm_reduced)$subj[,c("trial_ctr","trial_ctr:condScrambled")]))

ind_slopes_long <- pivot_longer(data = ind_slopes, cols = !c(subj), 
                                names_to = "cond", values_to = "slopes")

ind_slopes_long$cond <- factor(ind_slopes_long$cond, labels = levels(dat$cond))

### mittlere Effekte umrechnen 
# extrahiere posteriori Werte der Koeffizienten
posterior_samples <- as_draws(fm3_bayes, variable = c("b_snr_ctr", "b_snr_ctr:condScrambled"))
foo <- as_draws(fm3_bayes)
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


gg <- ggplot(ind_slopes_long, aes(x = cond, y = slopes, col = cond, fill = cond)) +
  geom_violinhalf(alpha = 0.1) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  scale_color_manual(values = okabe[c(6,8)]) +
  geom_boxplot(width = 0.1, size= 0.25, outlier.size = 0.5, col = "black", fill = "white") +
  labs(title = "Component scores", y = "Difference score", x = "Condition") +
  theme(legend.position = "bottom")
gg
ggsave("fig_score.pdf", device = "pdf", width = 10 / 2.54, height = 11.25 / 2.54)
