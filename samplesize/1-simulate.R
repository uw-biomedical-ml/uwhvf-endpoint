#!/usr/bin/env Rscript

library(survival)
library(pwr)

dat <- read.csv("../ptlvl.tsv", sep="\t")
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
            moddat <- read.csv(paste("model-", yr, ".csv", sep=""))
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
        for (effectsize in seq(0.10, 0.30, 0.01)) {
            prob2 <- (1 - effectsize) * prob
            h <- 2*asin(sqrt(prob)) - 2*asin(sqrt(prob2)) 
            pwr <- pwr.2p.test(h = h, sig.level=0.05, power=0.8)
            allres <- rbind(allres, data.frame(setting=setting, length=yr, control.event = prob, drug.event = prob2, sample.size = pwr$n, effect.size=effectsize))
        }
    }
}
write.csv(allres, "simulation.csv", row.names=F)
