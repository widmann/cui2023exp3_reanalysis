library(brms)
library(loo)
options(mc.cores = 4)

load(file = "data.Rdata")

#### SNR x cond ----
m1a <- pa_ctr ~ snr_ctr * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm1a_bayes <- brm(m1a, data = dat, cores = 4, iter = 100000, control = list(adapt_delta = 0.9),
                  save_pars = save_pars(all = TRUE))

# Warning messages:
# 1: There were 3 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them. 
# 2: Examine the pairs() plot to diagnose sampling problems

# Warning message:
#   There were 1 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 

summary(fm1a_bayes)
# plot(fm1a_bayes)
# pp_check(fm1a_bayes, ndraws = 100)


#### SNR x cond + trial_ctr x cond ----
m2a <- pa_ctr ~ snr_ctr * cond + trial_ctr * cond + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm2a_bayes <- brm(m2a, data = dat, cores = 4, iter = 100000, control = list(adapt_delta = 0.9),
                  save_pars = save_pars(all = TRUE))

summary(fm2a_bayes)
# plot(fm2a_bayes)
# pp_check(fm2a_bayes, ndraws = 100)


#### SNR x cond x trial_ctr ----
m3a <- pa_ctr ~ snr_ctr * cond * trial_ctr + I(trial_ctr ^ 2) * cond + (1 + snr_ctr * cond + trial_ctr + I(trial_ctr ^ 2) | subj)
fm3a_bayes <- brm(m3a, data = dat, cores = 4, iter = 100000, control = list(adapt_delta = 0.9),
                  save_pars = save_pars(all = TRUE))

get_prior(m3a,data = dat)

summary(fm3a_bayes)
# plot(fm3a_bayes)
# pp_check(fm3a_bayes, ndraws = 100)

save(file = "bayes_models.Rdata", fm1a_bayes, fm2a_bayes, fm3a_bayes)

# Clarke's third law: "Any sufficiently advanced technology is indistinguishable from magic."

fm1a_bayes <- add_criterion(fm1a_bayes, criterion = c("loo", "waic"), reloo = T)
fm2a_bayes <- add_criterion(fm2a_bayes, criterion = c("loo", "waic"), reloo = T)
fm3a_bayes <- add_criterion(fm3a_bayes, criterion = c("loo", "waic"), reloo = T)

save(file = "bayes_models.Rdata", fm1a_bayes, fm2a_bayes, fm3a_bayes)

#### Compare models
loo_compare(fm1a_bayes, fm2a_bayes, fm3a_bayes)

mw <- model_weights(fm1a_bayes, fm2a_bayes, fm3a_bayes)
mw
mw[3]/mw[2]

bayes_factor(fm2a_bayes,fm1a_bayes)
bayes_factor(fm3a_bayes,fm2a_bayes)
