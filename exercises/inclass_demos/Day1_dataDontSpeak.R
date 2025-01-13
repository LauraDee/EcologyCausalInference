## Day 1: Data Do Not Speak For Themselves
# LDee Spring 2025 EBIO

# This code and exercise is all From https://www.r-causal.org/chapters/05-not-just-a-stats-problem

library(ggplot2)
library(quartets)
library(datasauRus)
library(tidyverse)

#Anscombe
anscombe_quartet |>
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~dataset)

# roughly the same correlation in each dataset
datasaurus_dozen |>
  group_by(dataset) |>
  summarize(cor = round(cor(x, y), 2))

# yet very different causal structures
datasaurus_dozen |>
  ggplot(aes(x, y)) +
  geom_point() +
  facet_wrap(~dataset)

# visualizations don't reveal the difference in causal structures
causal_quartet |>
  # hide the dataset names
  mutate(dataset = as.integer(factor(dataset))) |>
  group_by(dataset) |>
  mutate(exposure = scale(exposure), outcome = scale(outcome)) |>
  ungroup() |>
  ggplot(aes(exposure, outcome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~dataset)
