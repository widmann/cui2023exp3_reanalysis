# This script reproduces the linear mixed models of the manuscript "Pupillometry
# is sensitive to speech masking during story listening: a commentary on the
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

#### Reduced model (without SNR) to estimate the cond x trial random effects reported in Fig. 1B
m_reduced <- pa ~ trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + cond * trial_ctr + cond * I(trial_ctr ^ 2) | subj)
m_reduced_bayes <- brm(m_reduced, data = dat, cores = 4, iter = 10000, save_pars = save_pars(all = TRUE))

summary(m_reduced_bayes)

# Check model convergence
plot(m_reduced_bayes)
pp_check(m_reduced_bayes, ndraws = 100)

#### Model 1: SNR x cond ----
m1 <- pupil_area ~ snr_ctr * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm1_bayes <- brm(m1, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm1_bayes)

# Check model convergence
plot(fm1_bayes)
pp_check(fm1_bayes, ndraws = 100)

#### Model 2: SNR x cond + trial x cond ----
m2 <- pupil_area ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2_bayes <- brm(m2, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm2_bayes)

# Check model convergence
plot(fm2_bayes)
pp_check(fm2_bayes, ndraws = 100)

#### Model 3: SNR x trial x cond ----
m3 <- pupil_area ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_bayes <- brm(m3, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm3_bayes)

# Check model convergence
plot(fm3_bayes)
pp_check(fm3_bayes, ndraws = 100)

#### Model 3 for fixation duration: SNR x cond x trial_ctr ----
m3_fd <- fix_dur ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3_fd_bayes <- brm(m3_fd, data = dat, cores = 4, iter = 25000, save_pars = save_pars(all = TRUE))

summary(fm3_fd_bayes)

# Check model convergence
plot(fm3_fd_bayes)
pp_check(fm3_fd_bayes, ndraws = 100)

# Note that we did not upload the Bayesian LMMs to Github because the file size
# was > 1 GB. The file is available on request from the authors. The full model
# output can be found in report_bayes_models.html.
save(file = "bayes_models.Rdata", fm1_bayes, fm2_bayes, fm3_bayes, fm3_fd_bayes, m_reduced_bayes)
