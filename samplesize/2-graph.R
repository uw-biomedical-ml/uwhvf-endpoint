#!/usr/bin/env Rscript

library(ggplot2)

pow80 <- read.csv("simulation.csv")

pow80$setting <- factor(pow80$setting, levels=c("base", "subgroup", "model"), labels=c("No selection", "Traditional Subgroup", "AI Model"))
pow80$effect.size <- 100.0 * pow80$effect.size
pow80 <- pow80[which(pow80$effect.size <= 30),]
pow80$Years <- factor(pow80$length)
p <- ggplot(pow80, aes(effect.size, sample.size, color=setting, group=setting)) + geom_point() + geom_line() + 
    scale_color_brewer(palette="Set1") + xlab("Effect size as percent reduction in event rate") + 
    ylab("Number of patients needed in each arm") + 
    scale_y_log10(breaks=c(300, 500, 1000, 2000, 5000, 10000, 20000, 40000, 80000, 160000)) + 
    theme(legend.title = element_blank()) +
    facet_wrap(Years~., scales="free_y") 
ggsave("pow80.png", p, width=12, height=6)


effect20 <- pow80[which(pow80$effect.size == 20 | pow80$effect.size == 10 | pow80$effect.size == 30 | pow80$effect.size == 50),]
p <- ggplot(effect20, aes(Years, sample.size, color=setting, group=setting)) + geom_point() + geom_line() +
    scale_color_brewer(palette="Set1") + ylim(0,10000) + facet_wrap(effect.size ~. )
ggsave("effect20.png", p, width=7, height=7)

quit()

p <- ggplot(sdat, aes(hazard.ratio, power, color=log(simn))) + geom_point() + facet_grid(sample.size ~ length)

ggsave("power.png", p, width=12, height=6)



quit()
dat <- read.csv("simultation.csv")
summary(dat)

p <- ggplot(dat, aes(effect.size, p)) + geom_point(alpha=0.2) + scale_color_brewer(palette="Set1") + facet_grid(sample.size ~ length)

ggsave("raw.png", p, width=12, height=6)

