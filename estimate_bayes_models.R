library(brms)
library(loo)
options(mc.cores = 4)

load(file = "data.Rdata")

contrasts(dat$cond) <- contr.sum(2)

# cond x trial_ctr
m_reduced <- pa ~ trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond * trial_ctr + cond * I(trial_ctr ^ 2) | subj)
m_reduced_bayes <- brm(m_reduced, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))

#### SNR x cond ----
m1 <- pa ~ snr_ctr * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm1_bayes <- brm(m1, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

# Warnmeldungen:
#   1: There were 6 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: Examine the pairs() plot to diagnose sampling problems

summary(fm1_bayes)
# plot(fm1_bayes)
# pp_check(fm1_bayes, ndraws = 100)


#### SNR x cond + trial_ctr x cond ----
m2 <- pa ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2_bayes <- brm(m2, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm2_bayes)
# plot(fm2_bayes)
# pp_check(fm2_bayes, ndraws = 100)


#### SNR x cond x trial_ctr ----
m3 <- pa ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_bayes <- brm(m3, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm3_bayes)
# plot(fm3_bayes)
# pp_check(fm3_bayes, ndraws = 100)


#### Fixation duration: SNR x cond x trial_ctr ----
m3_fd <- fd ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_fd_bayes <- brm(m3_fd, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))
summary(fm3_fd_bayes)


#### Spatial gaze dispersion: SNR x cond x trial_ctr ----
m3_sgdi <- sgd_i ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_sgdi_bayes <- brm(m3_sgdi, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))
summary(fm3_sgdi_bayes)

save(file = "bayes_models.Rdata", fm1_bayes, fm2_bayes, fm3_bayes, fm3_fd_bayes, fm3_sgdi_bayes, m_reduced_bayes)

load(file = "bayes_models.Rdata")
     
# fm1_bayes <- add_criterion(fm1_bayes, criterion = c("loo", "waic"), reloo = T)
# fm2_bayes <- add_criterion(fm2_bayes, criterion = c("loo", "waic"), reloo = T)
# fm3_bayes <- add_criterion(fm3_bayes, criterion = c("loo", "waic"), reloo = T)
fm1_bayes <- add_criterion(fm1_bayes, criterion = c("waic"))
fm2_bayes <- add_criterion(fm2_bayes, criterion = c("waic"))
fm3_bayes <- add_criterion(fm3_bayes, criterion = c("waic"))

save(file = "bayes_models_effect_coding.Rdata", fm1_bayes, fm2_bayes, fm3_bayes, fm3_fd_bayes, fm3_sgdi_bayes, m_reduced_bayes)
# save(file = "bayes_models.Rdata", fm1_bayes, fm2_bayes, fm3_bayes, m_reduced_bayes)

#### Compare models
# loo_compare(fm1_bayes, fm2_bayes, fm3_bayes)
waic(fm2_bayes, fm1_bayes)
waic(fm3_bayes, fm2_bayes)

# mw <- model_weights(fm1_bayes, fm2_bayes, fm3_bayes)
mw <- model_weights(fm2_bayes, fm3_bayes)
mw
mw[2]/mw[1]

bayes_factor(fm2_bayes, fm1_bayes)
set.seed(1)
bayes_factor(fm3_bayes, fm2_bayes)

bf_32 <- list()
for(seed in 0:99) {
  print(seed)
  set.seed(seed)
  bf_32[[seed + 1]] <- bayes_factor(fm3_bayes, fm2_bayes, maxiter = 1000)
}
save(file = "bf_1k.Rdata", bf_32)
