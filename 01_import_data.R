# This script imports the data for the replication of linear mixed models of the
# manuscript "Pupillometry is sensitive to speech masking during story
# listening: a commentary on the critical role of modeling temporal trends" by
# Andreas Widmann, Björn Herrmann, and Florian Scharf.
# 
# Authors: Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Andreas Widmann, Leipzig University

# Import data
dat <- read.csv(file = "eyedata.csv")

# Select columns for analysis
dat <- dat[, c(1:8, 13)]
colnames(dat)[8:9] <- c("pupil_area", "fix_dur")

# Convert to categorical variables (dummy coding)
dat$subj <- factor(dat$subj)
dat$cond <- factor(dat$cond, labels = c("Intact", "Scrambled"))
dat$block <- factor(dat$block)

# Mean center continuous covariate predictors
dat$snr_ctr <- dat$snr - 3
dat$trial_ctr <- dat$trial - 11.5

# Save data in R format
save(file = "data.Rdata", dat)
