# This script reproduces Fig. 2 of the manuscript "Pupillometry is sensitive to
# speech masking during story listening: The critical role of modeling temporal
# trends" by Andreas Widmann, Björn Herrmann, and Florian Scharf.
#
# Authors: Andreas Widmann, widmann@uni-leipzig.de and Florian Scharf, florian.scharf@uni-kassel.de
# Copyright (c) 2024 Andreas Widmann, Leipzig University and Florian Scharf, University of Kassel

library(ggplot2)
library(emmeans)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cuiCol <- c("#4e78f9", "#f73371")

# Import data
load("data.Rdata")
load("bayes_models.Rdata")

#### Panel A: SNR x cond x trial_ctr ----

emm_df <- rbind(emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(-10.5,-5.25), snr_ctr = -2:2), plotit = F, CIs = T),
                emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(-5.25,0), snr_ctr = -2:2), plotit = F, CIs = T),
                emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(0,5.25), snr_ctr = -2:2), plotit = F, CIs = T),
                emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(5.25,10.5), snr_ctr = -2:2), plotit = F, CIs = T))


emm_df$range = rep(1:4, each = 10)
emm_df$snr = emm_df$snr_ctr * 5 + 6
emm_df$range = factor(emm_df$range, labels = c("1st quarter of block", "2nd quarter of block", "3rd quarter of block", "4th quarter of block"))

ggplot(emm_df, aes(x = snr, y = yvar, col = cond)) +
  geom_ribbon(aes(min = LCL, max = UCL, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(shape = 16, size = 3) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  scale_color_manual(values = okabe[c(6,8)]) +
  facet_wrap(~ range, ncol = 5) +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
  theme(legend.position = "bottom")
# ggsave("fig3a.pdf", device = "pdf", width = 16 / 2.54, height = 7 / 2.54)

#### Panel B: SNR slope x cond x trial_ctr ----

emt <- emtrends(fm3_bayes, ~ cond * trial_ctr, var = "snr_ctr", at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2))
emmeans(emt, ~ cond * trial_ctr)
emt_df <- emmip(emt, ~ trial_ctr + cond, plotit = F, CIs = T)
emt_df$trial = emt_df$trial_ctr + 11.5

ggplot(emt_df, aes(x = trial, y = yvar, col = cond)) +
  geom_ribbon(aes(min = LCL, max = UCL, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  geom_point(shape = 16, size = 3) +
  scale_color_manual(values = okabe[c(6,8)]) +
  scale_fill_manual(values = okabe[c(6,8)]) +
  scale_x_continuous(limits=c(0.75, 22.25), expand = c(0, 0), breaks = seq(1,22,1)) +
  scale_y_continuous(breaks = seq(-30,20,10)) +
  facet_wrap(~ "Slope x Trial x Condition") +
  labs(color = "Condition", fill = "Condition") +
  labs(x = "Trial #", y = "Change in pupil area/+5 dB SNR [a.u.]") +
  theme(legend.position = "bottom")
# ggsave("fig3b.pdf", device = "pdf", width = 15.841 / 2.54, height = 7 / 2.54)
