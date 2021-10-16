#!/usr/bin/env Rscript

library(ggplot2)
dat <- read.csv("model.csv")

p <- ggplot(dat, aes(factor(status), X1, color=time)) + geom_point(position="jitter")
ggsave("coxrr.png", p)
