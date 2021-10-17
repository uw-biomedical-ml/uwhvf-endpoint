#!/usr/bin/env Rscript

library(survival)
library(pwr)

dat <- read.csv("../ptlvl.tsv", sep="\t")
dat <- dat[which(dat$grp == "test"),]

allres <- NULL
for (setting in c("base", "subgroup")) {
    if (setting == "base") {
        sdat <- data.frame(dat)
    } else if (setting == "subgroup") {
        sdat <- dat[which(dat$age >= 60 & dat$age < 80 & dat$startmd <= -5.0),]
    }

    mod <- survfit(Surv(time, event) ~ 1, data=sdat)
    eventrates <- summary(mod, times = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5))

    for (i in seq(length(eventrates$time))) {
        yr <- eventrates$time[i]
        prob <- 1 - eventrates$surv[i]
        for (effectsize in seq(0.10, 0.60, 0.05)) {
            prob2 <- (1 - effectsize) * prob
            h <- 2*asin(sqrt(prob)) - 2*asin(sqrt(prob2)) 
            pwr <- pwr.2p.test(h = h, sig.level=0.05, power=0.8)
            allres <- rbind(allres, data.frame(setting=setting, length=yr, control.event = prob, drug.event = prob2, sample.size = pwr$n, effect.size=effectsize))
        }
    }
}
write.csv(allres, "simulation.csv", row.names=F)
