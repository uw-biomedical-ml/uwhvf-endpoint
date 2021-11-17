#!/usr/bin/env Rscript

library(ggplot2)
library(survival)
library(pwr)

dat <- read.csv("data/ptlvl.tsv", sep="\t")
dat <- dat[which(dat$grp == "test"),]

allres <- NULL
times <- seq(1,5,0.5)

for (setting in c("model", "base", "subgroup")) {
    if (setting == "base") {
        sdat <- data.frame(dat)
    } else if (setting == "subgroup") {
        sdat <- dat[which(dat$age >= 60 & dat$age < 80 & dat$startmd <= -5.0),]
    }

    if (setting == "model") {
        eventrates <- data.frame(time = times)
        eventrates$surv <- NULL
        for (yr in times) {
            moddat <- read.csv(paste("data/model-", yr, ".csv", sep=""))
            moddat <- moddat[which(moddat$include == 1),]
            mod <- survfit(Surv(time, status) ~ 1, data=moddat)
            out <- summary(mod, times = yr)
            eventrates$surv[eventrates$time == yr] <- out$surv[1]
        }
    } else {
        mod <- survfit(Surv(time, event) ~ 1, data=sdat)
        eventrates <- summary(mod, times = times)
    }
    print(setting)
    print(eventrates)


    for (i in seq(length(eventrates$time))) {
        yr <- eventrates$time[i]
        prob <- 1 - eventrates$surv[i]
        for (effectsize in seq(0.10, 0.50, 0.10)) {
            prob2 <- (1 - effectsize) * prob
            #h <- 2*asin(sqrt(prob)) - 2*asin(sqrt(prob2)) 
            pwr <- pwr.2p.test(h = ES.h(prob, prob2), sig.level=0.05, power=0.8)
            allres <- rbind(allres, data.frame(setting=setting, length=yr, control.event = prob, drug.event = prob2, sample.size = pwr$n, effect.size=effectsize))
        }
    }
}
write.csv(allres, "data/simulation.csv", row.names=F)

pow80 <- allres
pow80$setting <- factor(pow80$setting, levels=c("base", "subgroup", "model"), labels=c("No selection", "Traditional Subgroup", "AI Model"))
pow80$effect.size <- 100.0 * pow80$effect.size
pow80$effect.size <- factor(pow80$effect.size)
pow80$Years <- factor(pow80$length)

effect20 <- pow80[which(pow80$effect.size == 20 | pow80$effect.size == 10 | pow80$effect.size == 30 | pow80$effect.size == 50),]
effect20$effect.size <- factor(effect20$effect.size, levels=c(10, 20, 30, 50), labels=c("Effect size: 10%", "Effect size: 20%", "Effect size: 30%", "Effect size: 50%"))
p <- ggplot(effect20, aes(Years, sample.size, color=setting, group=setting)) + geom_point() + geom_line() +
    scale_color_brewer(palette="Set1") + facet_wrap(~effect.size ) + xlab("Length of clinical trial (years)") + coord_cartesian(ylim=c(0,10000)) +
        ylab("Number of patients needed in each arm") + theme(legend.title = element_blank())
ggsave("figures/effect20.pdf", p, width=12, height=6)




