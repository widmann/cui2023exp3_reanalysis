# This script reproduces the Bayesian linear mixed models of the manuscript
# "Pupillometry is sensitive to speech masking during story listening: The
# critical role of modeling temporal trends" by Andreas Widmann, Björn Herrmann,
# and Florian Scharf.
#
# Authors: Florian Scharf, florian.scharf@uni-kassel.de and Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Florian Scharf, University of Kassel and Andreas Widmann, Leipzig University

library(brms)

rm(list = ls())
options(scipen = 4, width = 100)
options(mc.cores = 4)

load(file = "data.Rdata")

#### cond x trial_ctr
m_reduced <- pa ~ trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond * trial_ctr + cond * I(trial_ctr ^ 2) | subj)
m_reduced_bayes <- brm(m_reduced, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))

summary(m_reduced_bayes)
plot(m_reduced_bayes)
pp_check(m_reduced_bayes, ndraws = 100)

#### SNR x cond ----
m1 <- pa ~ snr_ctr * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm1_bayes <- brm(m1, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm1_bayes)
plot(fm1_bayes)
pp_check(fm1_bayes, ndraws = 100)

#### SNR x cond + trial_ctr x cond ----
m2 <- pa ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2_bayes <- brm(m2, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm2_bayes)
plot(fm2_bayes)
pp_check(fm2_bayes, ndraws = 100)

#### SNR x cond x trial_ctr ----
m3 <- pa ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_bayes <- brm(m3, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm3_bayes)
plot(fm3_bayes)
pp_check(fm3_bayes, ndraws = 100)

#### Fixation duration: SNR x cond x trial_ctr ----
m3_fd <- fd ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_fd_bayes <- brm(m3_fd, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm3_fd_bayes)
plot(fm3_fd_bayes)
pp_check(fm3_fd_bayes, ndraws = 100)

# Note that we did not upload the Bayesian LMMs to Github because the file size
# was > 1 GB. The file is available on request from the authors.
save(file = "bayes_models.Rdata", fm1_bayes, fm2_bayes, fm3_bayes, fm3_fd_bayes, m_reduced_bayes)
