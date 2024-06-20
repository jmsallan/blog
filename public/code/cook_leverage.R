## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)


## -----------------------------------------------------------------------------
library(tidyverse)
library(broom)


## ---- echo=FALSE--------------------------------------------------------------
set.seed(1414)
n <- 100
x <- rnorm(n)
y <- x + rnorm(n, sd=0.3)
label <- rep("normal", 100)

data1 <- data.frame(x = x, y = y, color = 1, label = rep("normal", 100))

data2 <- data.frame(x = c(0, 0, 5, 5), y = c(0, 5, 5, 0), color = 2, label = LETTERS[1:4])

data <- bind_rows(data1, data2)

ggplot(data, aes(x, y, color = factor(color))) +
  geom_point(size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#808080") +
  theme_classic() +
  geom_text(data = data2, aes(x, y, label = label), nudge_x = 0.25, color = "black", size = 5) +
  scale_color_manual(values = c("#FF6666", "#6666FF")) +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
mod <- lm(y ~ x, data)


## -----------------------------------------------------------------------------
augment(mod)


## ---- echo=FALSE--------------------------------------------------------------
data <- bind_cols(data, augment(mod) %>% select(starts_with(".")))
ggplot(data, aes(.hat, .cooksd, color = factor(color))) +
  geom_point() +
  geom_text(data = data %>% filter(label != "normal"), aes(.hat, .cooksd, label = label), color = "black", nudge_x = 0.003, size = 5) +
  theme_classic() +
  scale_color_manual(values = c("#FF6666", "#6666FF")) +
  labs(x = "leverage", y = "influence") +
  theme(legend.position = "none")


## -----------------------------------------------------------------------------
library(olsrr)


## -----------------------------------------------------------------------------
ols_plot_cooksd_chart(mod)


## -----------------------------------------------------------------------------
ols_plot_dfbetas(mod)

