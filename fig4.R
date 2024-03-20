# This script reproduces Fig. 4 of the manuscript "Pupillometry is sensitive to
# speech masking during story listening: The critical role of modeling temporal
# trends" by Andreas Widmann, Björn Herrmann, and Florian Scharf.
#
# Authors: Florian Scharf, florian.scharf@uni-kassel.de and Andreas Widmann, widmann@uni-leipzig.de
# Copyright (c) 2024 Florian Scharf, University of Kassel and Andreas Widmann, Leipzig University

library(ggplot2)

options(scipen = 4, width = 100)
rm(list = ls())
theme_set(theme_gray(base_size = 10))
okabe <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Simulated data first scenario (no condition by trend interaction) ----

simdata <- data.frame(Trial = rep(1:100, 2))
simdata$Condition <- factor(c(rep("Condition A", 100), rep("Condition B", 100)))
simdata$Outcome <- c(exp(-simdata[simdata$Condition == "Condition A",]$Trial / 10 * 0.8) * 5 + 3,
                     exp(-simdata[simdata$Condition == "Condition B",]$Trial / 10 * 0.8) * 5)

# Panel A
ggplot(data = simdata, aes(x = Trial, y = Outcome, color = Condition)) +
  geom_line() +
  scale_color_manual(values = okabe[c(3, 4)]) +
  facet_grid(~ "Trends within conditions") +
  theme(legend.position = "bottom") +
  labs(color = element_blank()) +
  ylim(0, 10)
# ggsave("fig4a.pdf", device = "pdf", width = 8 / 2.54, height = 7.2 / 2.54)

# Panel B
simeffect <- data.frame(Trial = rep(1:100, 2))
simeffect$Effect <- factor(c(rep("Conditional mean", 100), rep("Marginal mean", 100)))
simeffect$"Condition effect" <- c(simdata[simdata$Condition == "Condition A",]$Outcome - simdata[simdata$Condition == "Condition B",]$Outcome,
                                (cumsum(simdata[simdata$Condition == "Condition A",]$Outcome) - cumsum(simdata[simdata$Condition == "Condition B",]$Outcome)) / cummax(simdata[simdata$Condition == "Condition A",]$Trial))

ggplot(data = simeffect, aes(x = Trial, y = `Condition effect`, color = Effect, linetype = Effect)) +
  geom_line() +
  scale_color_manual(values = okabe[c(2, 6)]) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  facet_grid(~ "Trends of condition effect") +
  theme(legend.position = "bottom") +
  labs(color = element_blank(), linetype = element_blank()) +
  ylim(0, 10)
# ggsave("fig4b.pdf", device = "pdf", width = 8 / 2.54, height = 7.2 / 2.54)

#### Simulated data second scenario (no condition by trend interaction) ----
simdata <- data.frame(Trial = rep(1:100, 2))
simdata$Condition <- factor(c(rep("Condition A", 100), rep("Condition B", 100)))
simdata$Outcome <- c(exp(-simdata[simdata$Condition == "Condition A",]$Trial / 10 * 0.8) * 10,
                     exp(-simdata[simdata$Condition == "Condition B",]$Trial / 10 * 0.8) * 2)

# Panel C
ggplot(data = simdata, aes(x = Trial, y = Outcome, color = Condition)) +
  geom_line() +
  scale_color_manual(values = okabe[c(3, 4)]) +
  facet_grid(~ "Trends within conditions") +
  theme(legend.position = "bottom") +
  labs(color = element_blank()) +
  ylim(0, 10)
# ggsave("fig4c.pdf", device = "pdf", width = 8 / 2.54, height = 7.2 / 2.54)

# Panel D
simeffect <- data.frame(Trial = rep(1:100, 2))
simeffect$Effect <- factor(c(rep("Conditional mean", 100), rep("Marginal mean", 100)))
simeffect$"Condition effect" <- c(simdata[simdata$Condition == "Condition A",]$Outcome - simdata[simdata$Condition == "Condition B",]$Outcome,
                                  (cumsum(simdata[simdata$Condition == "Condition A",]$Outcome) - cumsum(simdata[simdata$Condition == "Condition B",]$Outcome)) / cummax(simdata[simdata$Condition == "Condition A",]$Trial))

ggplot(data = simeffect, aes(x = Trial, y = `Condition effect`, color = Effect, linetype = Effect)) +
  geom_line() +
  scale_color_manual(values = okabe[c(2, 6)]) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  facet_grid(~ "Trends of condition effect") +
  theme(legend.position = "bottom") +
  labs(color = element_blank(), linetype = element_blank()) +
  ylim(0, 10)
# ggsave("fig4d.pdf", device = "pdf", width = 8 / 2.54, height = 7.2 / 2.54)
