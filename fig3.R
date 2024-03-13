library(ggplot2)
library(emmeans)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# cuiCol <- c("#4e78f9", "#f73371")

# Import data
load("data.Rdata")

# (2) Dann würde ich gerne möglichst analog zu Fig. 9 im Cui-Paper zeigen, wie es aussähe,
#  wenn man es modelliert und Trial als Kovariate ins Modell aufnimmt. 
#  Botschaft: Pupille ist sensitiv für SNR/effort.

#### @ Andreas: Keine Ahnung, wie man die y-Achse sinnvoll skaliert hier, mir fehlt
#### auch dazu die Info, warum die AU bei uns so anders ist als bei denen

load("bayes_models.Rdata")

#### Panel A: SNR x cond x trial_ctr

# (3) Und dann würde ich gerne zugänglich visualisieren, wie sich der Effekt im Zeitverlauf entwickelt,
#  wenn man die Interaktion zulässt. Botschaft: Pupille ist sogar so sensitiv, 
#  dass sich die Veränderung des Effektes im Verlauf des Experimentes auswerten lässt.

# aus dem polynomialen Modell heraus:
# summary(fm3_bayes)
# # conditions <- make_conditions(fm3_bayes, vars = c("trial_ctr"))
# conditions = cbind(trial_ctr = c(-7.5,-3.5,0.5,4.5,8.5), cond__ = c(-7.5,-3.5,0.5,4.5,8.5) + 10.5)
# my_plot <- conditional_effects(fm3_bayes, effects = "snr_ctr:cond", 
#                                conditions = conditions,
#                                # re_formula = NULL)
#                                re_formula = NA)
# plot(my_plot, plot = FALSE)[[1]] +
#   scale_fill_manual(values = okabe[c(6,8)]) +
#   scale_color_manual(values = okabe[c(6,8)]) +
#   facet_wrap(~trial_ctr, ncol = 5) +
#   labs(color = "Condition", fill = "Condition") +
#   labs(x = "SNR [dB]", y = "Predicted pupil area [a.u.]") +
#   theme(legend.position = "bottom")

load("ind_fits_from_f2.Rdata")

emm_df <- rbind(emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(-10.5,-5.25), snr_ctr = -2:2), plotit = F, CIs = T),
                emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(-5.25,0), snr_ctr = -2:2), plotit = F, CIs = T),
                emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(0,5.25), snr_ctr = -2:2), plotit = F, CIs = T),
                emmip(fm3_bayes, cond ~ snr_ctr, at = list(trial_ctr = c(5.15,10.5), snr_ctr = -2:2), plotit = F, CIs = T))


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
ggsave("fig3a.pdf", device = "pdf", width = 16 / 2.54, height = 7 / 2.54)

#### Panel B: SNR slope x cond x trial_ctr

emt <- emtrends(fm3_bayes, ~ cond * trial_ctr, var = "snr_ctr", at = list(trial_ctr=-10.5:10.5, snr_ctr=-2:2))
# emt <- emtrends(fm3_bayes, ~ cond * trial_ctr, var = "snr_ctr", at = list(trial_ctr=c(-7.875,-2.625,2.625,7.875), snr_ctr=-2:2))
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
ggsave("fig3b.pdf", device = "pdf", width = 15.841 / 2.54, height = 7 / 2.54)








# 3a 
new_plot_dat1 <- aggregate(cbind(estimate__, lower__, upper__, se__) ~ snr_ctr + cond, ind_pred[ind_pred$trial_ctr <= -5.25,], mean)
new_plot_dat2 <- aggregate(cbind(estimate__, lower__, upper__, se__) ~ snr_ctr + cond, ind_pred[ind_pred$trial_ctr >= -5.25 & ind_pred$trial_ctr < 0,], mean)
new_plot_dat3 <- aggregate(cbind(estimate__, lower__, upper__, se__) ~ snr_ctr + cond, ind_pred[ind_pred$trial_ctr >= 0 & ind_pred$trial_ctr < 5.25,], mean)
new_plot_dat4 <- aggregate(cbind(estimate__, lower__, upper__, se__) ~ snr_ctr + cond, ind_pred[ind_pred$trial_ctr >= +5.15,], mean)

ggplot(new_plot_dat, aes(x = snr_ctr, y = estimate__, col = cond)) +
  geom_ribbon(aes(min = estimate__ - se__, max = estimate__ + se__, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  # facet_wrap(vars(cond)) +
  ylim(1200,1800)
ggplot(new_plot_dat2, aes(x = snr_ctr, y = estimate__, col = cond)) +
  geom_ribbon(aes(min = estimate__ - se__, max = estimate__ + se__, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  # facet_wrap(vars(cond)) +
  ylim(1200,1800)
ggplot(new_plot_dat3, aes(x = snr_ctr, y = estimate__, col = cond)) +
  geom_ribbon(aes(min = estimate__ - se__, max = estimate__ + se__, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  # facet_wrap(vars(cond)) +
  ylim(1200,1800)
ggplot(new_plot_dat4, aes(x = snr_ctr, y = estimate__, col = cond)) +
  geom_ribbon(aes(min = estimate__ - se__, max = estimate__ + se__, fill = cond), color = NA, alpha = 0.15) +
  geom_line() +
  # facet_wrap(vars(cond)) +
  ylim(1200,1800)

