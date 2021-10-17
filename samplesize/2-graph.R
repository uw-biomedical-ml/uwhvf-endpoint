#!/usr/bin/env Rscript

library(ggplot2)

pow80 <- read.csv("simulation.csv")

pow80$length <- factor(pow80$length)
p <- ggplot(pow80, aes(effect.size, sample.size, color=length, group=length)) + geom_point() + geom_smooth() + scale_color_brewer(palette="Set1")
ggsave("pow80.png", p)

quit()

p <- ggplot(sdat, aes(hazard.ratio, power, color=log(simn))) + geom_point() + facet_grid(sample.size ~ length)

ggsave("power.png", p, width=12, height=6)



quit()
dat <- read.csv("simultation.csv")
summary(dat)

p <- ggplot(dat, aes(effect.size, p)) + geom_point(alpha=0.2) + scale_color_brewer(palette="Set1") + facet_grid(sample.size ~ length)

ggsave("raw.png", p, width=12, height=6)

