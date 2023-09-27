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
  summarise(pa_sd = sd(pa), pa_mean = mean(pa))
n <- nlevels(dat$subj)
ggplot(dat_cond_trial, aes(x = trial, y = pa_mean, col = cond)) +
  geom_ribbon(aes(min = pa_mean - pa_sd / sqrt(n) * 1.96, max = pa_mean + pa_sd / sqrt(n) * 1.96, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(shape = 16, size = 2) +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")
ggsave("fig1a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

#### Panel B: Linear trend

load("bayes_models.Rdata")

ranef(m_reduced_bayes)$subj[,"Estimate",]
fixef(m_reduced_bayes)

### individuelle Effekte rausholen
ind_slopes <- data.frame(subj = unique(dat$subj),
                         slopes_intact = coef(m_reduced_bayes)$subj[,"Estimate", "trial_ctr"],
                         slopes_scrambled = rowSums(coef(m_reduced_bayes)$subj[,"Estimate", c("trial_ctr","condScrambled:trial_ctr")]))

ind_slopes_long <- pivot_longer(data = ind_slopes, cols = !c(subj), 
                                names_to = "cond", values_to = "slopes")

ind_slopes_long$cond <- factor(ind_slopes_long$cond, labels = levels(dat$cond))

### mittlere Effekte umrechnen 
# extrahiere posteriori Werte der Koeffizienten
posterior_samples <- as_draws(m_reduced_bayes, variable = c("b_trial_ctr", "b_trial_ctr:condScrambled"))
posterior_samples <- merge_chains(posterior_samples, variable = c("b_trial_ctr", "b_trial_ctr:condScrambled"))
posterior_samples <- as_draws_df(posterior_samples)

# Man kann aus der Posterior die KIs bestimmen, erstmal als Demo/Check
fixef(m_reduced_bayes)["trial_ctr",]
quantile(posterior_samples$b_trial_ctr, probs = c(0.025, 0.975))
# für den Effekt in der anderen Bedingung, muss man die Koeffizienten addieren
posterior_samples$b_trial_ctr_scrambled <- posterior_samples$b_trial_ctr + posterior_samples$`b_trial_ctr:condScrambled`

# berechne die mittleren slopes der beiden Bedingungen und generiere die KIs dafür
# aus den posteriori Quantilen
CIs <- apply(posterior_samples[,c("b_trial_ctr","b_trial_ctr_scrambled")], 2, quantile, probs = c(0.025, 0.975))
CIs

avg_slopes <- data.frame(cond = levels(dat$cond), 
                         slopes = colMeans(posterior_samples[,c("b_trial_ctr","b_trial_ctr_scrambled")]),
                         CIs = t(CIs))


ggplot(ind_slopes_long, aes(x = cond, y = slopes, group = subj, fill = cond, col = cond)) +
  geom_point(shape = 16, alpha = 0.4, size = 2) +
  geom_line(alpha = 0.4, col = "gray") +
  geom_point(shape = 16, data = avg_slopes, aes(group = NULL), size = 5) +
  geom_errorbar(data = avg_slopes, aes(group = NULL, min = CIs.2.5., max = CIs.97.5.), width = 0.1) +
  facet_grid(~ "Trial linear trend") +
  scale_color_manual(values = okabe[c(6,8)]) +
  labs(x = "Condition", y = "Change in pupil area/trial [a.u.]") +
  theme(legend.position = "bottom") 
ggsave("fig1b.pdf", device = "pdf", width = 4 / 2.54, height = 9 / 2.54)
