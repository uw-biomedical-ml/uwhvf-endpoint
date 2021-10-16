#!/usr/bin/env Rscript

library(ggplot2)

dat <- read.csv("power.csv")
p <- ggplot(dat, aes(hazard.ratio, power, color=simn)) + geom_point() + facet_grid(sample.size ~ length)

ggsave("power.png", p, width=12, height=6)


quit()
dat <- read.csv("simultation.csv")
summary(dat)

p <- ggplot(dat, aes(effect.size, p)) + geom_point(alpha=0.2) + scale_color_brewer(palette="Set1") + facet_grid(sample.size ~ length)

ggsave("raw.png", p, width=12, height=6)

