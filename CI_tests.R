library(knitr)
library(lme4)
library(here)
library(arm)
library(broom.mixed)
library(kableExtra)
library(patchwork)
library(brms)
library(tidyverse)

sleepstudy <- as_tibble(sleepstudy)

p1 <- ggplot(sleepstudy, aes(x = Days, y = Reaction)) +
  geom_point(shape = 1) +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  geom_smooth(method = "lm", fill = "dodgerblue", level = .95)
p2 <- p1 + facet_wrap(~Subject, nrow = 4)
p1 | p2

lmerfit <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)

newavg <- data.frame(Days = 0:9)
newavg$Reaction <- predict(lmerfit, re.form = NA, newavg)
# Predictors for the varying effect's predictions
newvary <- expand.grid(Days = 0:9, Subject = unique(sleepstudy$Subject))
newvary$Reaction <- predict(lmerfit, newvary)

p1 + geom_line(data = newavg, col = "black", size = 1) |
  p2 + geom_line(data = newvary, col = "black", size = 1)

sims <- sim(lmerfit, n.sims = 1000) # 1
fs <- fixef(sims) # 2
newavg <- data.frame(Days = 0:9)
Xmat <- model.matrix(~ 1 + Days, data = newavg) # 3
fitmat <- matrix(ncol = nrow(fs), nrow = nrow(newavg)) # 4
for (i in 1:nrow(fs)) {
  fitmat[, i] <- Xmat %*% as.matrix(fs)[i, ]
} # 5
newavg$lower <- apply(fitmat, 1, quantile, prob = 0.05) # 6
newavg$median <- apply(fitmat, 1, quantile, prob = 0.5) # 6
newavg$upper <- apply(fitmat, 1, quantile, prob = 0.95) # 6
p1 + geom_line(data = newavg, aes(y = median), size = 1) +
  geom_line(data = newavg, aes(y = lower), lty = 2) +
  geom_line(data = newavg, aes(y = upper), lty = 2)



p1_pa <- ggplot(dat, aes(x = snr_ctr, y = pa, group = cond, col = cond)) +
  geom_point(shape = 1) +
  scale_x_continuous(breaks = c(0, 3, 6, 9)) +
  geom_smooth(method = "lm", fill = "dodgerblue", level = .95)

p1_pa
