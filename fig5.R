# This script reproduces Fig. 5 of the manuscript "Pupillometry is sensitive to
# speech masking during story listening: The critical role of modeling temporal
# trends" by Andreas Widmann, Björn Herrmann, and Florian Scharf.
#
# Authors: Florian Scharf, florian.scharf@uni-kassel.de and Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Florian Scharf, University of Kassel and Andreas Widmann, Leipzig University

library(ggplot2)
library(margins)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Prepare simulated data
time <- 1:100
condition1 <- exp(-time / 10 * 0.8) * 5 + 2 
condition2 <- exp(-time / 10 * 0.8) * 5 + 1

set.seed(22)
x1 <- sample(rep(1:2, 50), size = 100)
y1 <- vector(length = 100)

for (i in 1:100){
  y1[i] <- ifelse(x1[i]==1, condition1[i], condition2[i]) + rnorm(n = 100, sd = 1)
}

simdata <- data.frame(Trial = time, Condition = x1, Outcome = y1)
simdata$Condition <- factor(simdata$Condition, labels = c("Condition A", "Condition B"))

#### Panel A ----
ggplot(data = simdata, aes(x = Trial, y = Outcome, color = Condition)) +
  geom_line(data = data.frame(Trial = 1:100, Outcome = condition1, Condition = "Condition A"), alpha = 0.5) +
  geom_line(data = data.frame(Trial = 1:100, Outcome = condition2, Condition = "Condition B"), alpha = 0.5) +
  geom_point(shape = 16, size = 1, alpha = 0.8) +
  scale_color_manual(values = okabe[c(4, 3)]) +
  facet_grid(~ "Before detrending") +
  theme(legend.position = "bottom") +
  labs(color = element_blank()) +
  ylim(0, 6)
# ggsave("fig5a-1.pdf", device = "pdf", width = 6.09 / 2.54, height = 7.2 / 2.54)

simdata$Trial_ctr <- as.numeric(scale(simdata$Trial, scale = F))
simdata$Condition <- relevel(simdata$Condition, ref = "Condition A")
fit_naive1 <- lm(Outcome ~ Condition, simdata)
fit_trend1 <- lm(Outcome ~ 1 + Condition + Trial_ctr + I(Trial_ctr^2) + I(Trial_ctr^3) + I(Trial_ctr^4), simdata)
simdata$Condition <- relevel(simdata$Condition, ref = "Condition B")
fit_naive2 <- lm(Outcome ~ Condition, simdata)
fit_trend2 <- lm(Outcome ~ 1 + Condition + Trial_ctr + I(Trial_ctr^2) + I(Trial_ctr^3) + I(Trial_ctr^4), simdata)

summary(fit_naive1)
summary(fit_trend2)

m_trend1 <- margins(fit_trend1)
m_trend2 <- margins(fit_trend2)

summary(m_trend1)
summary(m_trend2)

b_naive1 <- coef(fit_naive1)
b_naive2 <- coef(fit_naive2)

meandata <- data.frame(Condition = c("A", "B"), mean = c(b_naive1[1], b_naive2[1]), lcl = c(confint(fit_naive1)[1,1], confint(fit_naive2)[1,1]), ucl = c(confint(fit_naive1)[1,2], confint(fit_naive2)[1,2]))

ggplot(data = meandata, aes(x = Condition, y = mean, color = Condition)) +
  geom_point(shape = 15, size = 2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 1) +
  scale_color_manual(values = okabe[c(4, 3)]) +
  facet_grid(~ "Marginal means") +
  theme(legend.position = "bottom") +
  labs(color = element_blank()) +
  ylim(0, 6)
# ggsave("fig5a-2.pdf", device = "pdf", width = 2.35 / 2.54, height = 7.2 / 2.54)

# Plot again after correction for the trend

# create new outcome variable without trend
simdata$Condition <- relevel(simdata$Condition, ref = "Condition A")
fit_trend <- lm(Outcome ~ 1 + Trial_ctr + I(Trial_ctr^2) + I(Trial_ctr^3) + I(Trial_ctr^4), simdata)
simdata$Outcome_detrended <- mean(simdata$Outcome) + resid(fit_trend)

#### Panel B ----

ggplot(data = simdata, aes(x = Trial, y = Outcome_detrended, color = Condition)) +
  geom_point(shape = 16, size = 1.2) +
  scale_color_manual(values = okabe[c(4, 3)]) +
  facet_grid(~ "After detrending") +
  theme(legend.position = "bottom") +
  labs(color = element_blank()) +
  ylim(0, 6)
# ggsave("fig5b-1.pdf", device = "pdf", width = 6.09 / 2.54, height = 7.2 / 2.54)

simdata$Trial_ctr <- as.numeric(scale(simdata$Trial, scale = F))
simdata$Condition <- relevel(simdata$Condition, ref = "Condition A")
fit_naive1 <- lm(Outcome_detrended ~ Condition, simdata)
fit_trend1 <- lm(Outcome_detrended ~ 1 + Condition + Trial_ctr + I(Trial_ctr^2) + I(Trial_ctr^3) + I(Trial_ctr^4), simdata)
simdata$Condition <- relevel(simdata$Condition, ref = "Condition B")
fit_naive2 <- lm(Outcome_detrended ~ Condition, simdata)
fit_trend2 <- lm(Outcome_detrended ~ 1 + Condition + Trial_ctr + I(Trial_ctr^2) + I(Trial_ctr^3) + I(Trial_ctr^4), simdata)

summary(fit_naive1)
summary(fit_trend2)

m_trend1 <- margins(fit_trend1)
m_trend2 <- margins(fit_trend2)

summary(m_trend1)
summary(m_trend2)

b_naive1 <- coef(fit_naive1)
b_naive2 <- coef(fit_naive2)

meandata <- data.frame(Condition = c("A", "B"), mean = c(b_naive1[1], b_naive2[1]), lcl = c(confint(fit_naive1)[1,1], confint(fit_naive2)[1,1]), ucl = c(confint(fit_naive1)[1,2], confint(fit_naive2)[1,2]))

ggplot(data = meandata, aes(x = Condition, y = mean, color = Condition)) +
  geom_point(shape = 15, size = 2) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 1) +
  scale_color_manual(values = okabe[c(4, 3)]) +
  facet_grid(~ "Marginal means") +
  theme(legend.position = "bottom") +
  labs(color = element_blank()) +
  ylim(0, 6)
# ggsave("fig5b-2.pdf", device = "pdf", width = 2.35 / 2.54, height = 7.2 / 2.54)
