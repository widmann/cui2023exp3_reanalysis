# Authors: Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Andreas Widmann, Leipzig University

# Import data
dat <- read.csv(file = "eyedata.csv")
dat$subj <- factor(dat$subj)
dat$cond <- factor(dat$cond, labels = c("Intact", "Scrambled"))
dat$block <- factor(dat$block)
dat$snr_ctr <- dat$snr - 3
dat$trial_ctr <- dat$trial - 11.5

save(file = "data.Rdata", dat)