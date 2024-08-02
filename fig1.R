# This script reproduces Fig. 1 of the manuscript "Pupillometry is sensitive to
# speech masking during story listening: a commentary on the critical role of
# modeling temporal trends" by Andreas Widmann, Björn Herrmann, and Florian
# Scharf.
#
# Authors: Florian Scharf, florian.scharf@uni-kassel.de and Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Florian Scharf, University of Kassel and Andreas Widmann, Leipzig University

library(ggplot2)
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
load(file = "data.Rdata")

#### Panel A: Grand average time course

# Compute pupil area grand average per condition and trial over participants
dat_cond_trial <- dat %>%
  group_by(cond, trial) %>%
  summarise(pupil_area_sd = sd(pupil_area), pupil_area_mean = mean(pupil_area))

# Compute CIs
n <- nlevels(dat$subj)
dat_cond_trial$lcl <- dat_cond_trial$pupil_area_mean - dat_cond_trial$pupil_area_sd / sqrt(n) * qt(0.975,df = n - 1)
dat_cond_trial$ucl <- dat_cond_trial$pupil_area_mean + dat_cond_trial$pupil_area_sd / sqrt(n) * qt(0.975,df = n - 1)

# Plot pupil area grand average
ggplot(dat_cond_trial, aes(x = trial, y = pupil_area_mean, col = cond)) +
  geom_ribbon(aes(min = lcl, max = ucl, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(shape = 16, size = 2) +
  facet_grid(~ "Grand-average") +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Pupil area [a.u.]") +
  theme(legend.position = "bottom")
# ggsave("fig1a.pdf", device = "pdf", width = 12 / 2.54, height = 9 / 2.54)

#### Panel B: Linear trend

load("bayes_models.Rdata")

ranef(m_reduced_bayes)$subj[,"Estimate",]
fixef(m_reduced_bayes)

### compute individual condition effects from the LMM random effects 
ind_slopes <- data.frame(subj = unique(dat$subj),
                         slopes_intact = coef(m_reduced_bayes)$subj[,"Estimate", "trial_ctr"],
                         slopes_scrambled = rowSums(coef(m_reduced_bayes)$subj[,"Estimate", c("trial_ctr","condScrambled:trial_ctr")]))

ind_slopes_long <- pivot_longer(data = ind_slopes, cols = !c(subj), 
                                names_to = "cond", values_to = "slopes")

ind_slopes_long$cond <- factor(ind_slopes_long$cond, labels = levels(dat$cond))

### compute predicted values and CIs per condition
# extract posterior values for the LMM coefficients and merge all chains
posterior_samples <- as_draws(m_reduced_bayes, variable = c("b_trial_ctr", "b_trial_ctr:condScrambled"))
posterior_samples <- merge_chains(posterior_samples, variable = c("b_trial_ctr", "b_trial_ctr:condScrambled"))
posterior_samples <- as_draws_df(posterior_samples)

# in order to compute CIs from the posterior, one can use quantiles from the posterior
# for instance, this reproduces the CI for the trial effect from the summary
fixef(m_reduced_bayes)["trial_ctr",]
quantile(posterior_samples$b_trial_ctr, probs = c(0.025, 0.975))
# one can compute the trial effect for the scrambled condition by 
# adding the interaction coefficient to b_trial_ctr
# to get the posterior distribution of the trial effect in the scrambled condition
# we need to repeat this for all posterior samples and then we get the CIs for Figure 1
# 
posterior_samples$b_trial_ctr_scrambled <- posterior_samples$b_trial_ctr + posterior_samples$`b_trial_ctr:condScrambled`

CIs <- apply(posterior_samples[,c("b_trial_ctr","b_trial_ctr_scrambled")], 2, quantile, probs = c(0.025, 0.975))
CIs

avg_slopes <- data.frame(cond = levels(dat$cond), 
                         slopes = colMeans(posterior_samples[,c("b_trial_ctr","b_trial_ctr_scrambled")]),
                         CIs = t(CIs))

ggplot(ind_slopes_long, aes(x = cond, y = slopes, group = subj, fill = cond, col = cond)) +
  geom_point(shape = 16, alpha = 0.4, size = 2) +
  geom_line(alpha = 0.4, col = "gray") +
  geom_point(shape = 16, data = avg_slopes, aes(group = NULL), size = 5) +
  geom_errorbar(data = avg_slopes, aes(group = NULL, min = CIs.2.5., max = CIs.97.5.), width = 0.25) +
  facet_grid(~ "Trial linear trend") +
  scale_color_manual(values = okabe[c(6,8)]) +
  labs(x = "Condition", y = "Change in pupil area/trial [a.u.]") +
  theme(legend.position = "bottom") 
# ggsave("fig1b.pdf", device = "pdf", width = 4 / 2.54, height = 9 / 2.54)
