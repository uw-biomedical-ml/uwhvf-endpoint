#!/usr/bin/env Rscript

library(survival)
library(doMC)
registerDoMC(cores = 20)

dat <- read.csv("../ptlvl.tsv", sep="\t")
sdat <- dat[which(dat$age >= 60 & dat$age < 80 & dat$startmd <= -5.0),]

mod <- survfit(Surv(time, event) ~ 1, data=sdat)
eventrates <- summary(mod, times = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5))

print(eventrates)
print(names(eventrates))
print(eventrates$time)
print(eventrates$surv)

allres <- NULL
for (i in seq(length(eventrates$time))) {
    yr <- eventrates$time[i]
    prob <- 1 - eventrates$surv[i]
    for (effectsize in seq(0.10, 0.60, 0.05)) {
        ls = 50
        rs = 200000
        ss = 2000
        prob2 <- (1 - effectsize) * prob
        while (T) {
            print(c(yr, ss, effectsize))
            res <- foreach (rep=1:1000, .combine=rbind) %dopar% {
                control <- sum(runif(ss,0,1) < prob)
                exp <- sum(runif(ss,0,1) < prob2)
                ft <- fisher.test(matrix(c(control, ss - control, exp, ss - exp), nrow=2))
                ft$p.value
            }
            power <- 100.0 * sum(res < 0.05) / nrow(res)
            print(power)
            if (power >= 79.9 & power <= 80.1) { break }
            if (ls == rs) { break }
            if (power > 80.1) { rs = ss }
            if (power < 79.9) { ls = ss }
            ss = as.integer((ls + rs) / 2.0)
        }
        allres <- rbind(allres, data.frame(length=yr, control.event = prob, drug.event = prob2, sample.size = ss, effect.size=effectsize))
    }
}
write.csv(allres, "simulation.csv", row.names=F)
