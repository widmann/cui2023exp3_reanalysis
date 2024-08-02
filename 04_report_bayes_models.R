# This script reports the Bayesian linear mixed model output of the manuscript
# "Pupillometry is sensitive to speech masking during story listening: a
# commentary on the critical role of modeling temporal trends" by Andreas
# Widmann, Björn Herrmann, and Florian Scharf.
#
# Authors: Andreas Widmann, widmann@uni-leipzig.de and Florian Scharf, florian.scharf@uni-kassel.de
# Copyright (c) 2024 Andreas Widmann, Leipzig University and Florian Scharf, University of Kassel

library(brms)

rm(list = ls())
options(scipen = 4, width = 100)
options(mc.cores = 4)

# Load the Bayesian linear mixed models computed in 03_estimate_bayes_models.R
load(file = "bayes_models.Rdata")

# Report full model output
summary(m_reduced_bayes)
summary(fm1_bayes)
summary(fm2_bayes)
summary(fm3_bayes)

# Compute Bayes factors for model comparisons
# set.seed(1)
# bayes_factor(fm2_bayes, fm1_bayes)
# set.seed(1)
# bayes_factor(fm3_bayes, fm2_bayes)
