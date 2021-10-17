#!/usr/bin/env Rscript

library(ggplot2)

pow80 <- read.csv("simulation.csv")

pow80$effect.size <- 100.0 * pow80$effect.size
pow80 <- pow80[which(pow80$effect.size <= 30),]
pow80$Years <- factor(pow80$length)
p <- ggplot(pow80, aes(effect.size, sample.size, color=setting, group=setting)) + geom_point() + geom_line() + scale_color_brewer(palette="Set1") + xlab("Effect size as percent reduction in event rate") + ylab("Number of patients needed in each arm") + facet_grid(Years~., scales="free_y")
ggsave("pow80.png", p)

quit()

p <- ggplot(sdat, aes(hazard.ratio, power, color=log(simn))) + geom_point() + facet_grid(sample.size ~ length)

ggsave("power.png", p, width=12, height=6)



quit()
dat <- read.csv("simultation.csv")
summary(dat)

p <- ggplot(dat, aes(effect.size, p)) + geom_point(alpha=0.2) + scale_color_brewer(palette="Set1") + facet_grid(sample.size ~ length)

ggsave("raw.png", p, width=12, height=6)

