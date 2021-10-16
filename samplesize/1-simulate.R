#!/usr/bin/env Rscript

library(survival)
library(doMC)
registerDoMC(cores = 20)

dat <- read.csv("../ptlvl.tsv", sep="\t")
sdat <- dat[which(dat$age >= 60 & dat$age < 80 & dat$startmd <= -5.0),]
summary(sdat)


df <- NULL
for (ss in seq(50,1000,50)) {
    print(ss)
    for (yr in seq(1.0,5,0.5)) {
        print(yr)
        ysdat <- data.frame(sdat)
        ysdat$event[ysdat$time >= yr] <- 0
        ysdat$time[ysdat$time >= yr] <- yr
        res <- foreach (rep=1:100000, .combine=rbind) %dopar% {
        #for (rep in seq(10000)) {
            n1 <- ysdat[sample(nrow(ysdat),ss,replace=TRUE),]
            n2 <- ysdat[sample(nrow(ysdat),ss,replace=TRUE),]
            n3 <- data.frame(n2)
            indx <- which(n3$event == 1)
            if (length(indx) > 1) {
                flipindx <- sample(indx, sample(1:length(indx),1), replace=F)
                n3$event[flipindx] <- 0
            }

            # measure hr effect of flip
            n1$g <- "Control"
            n2$g <- "Control"
            n3$g <- "Exp"
            full <- rbind(n2, n3)
            full$g <- factor(full$g, levels=c("Control", "Exp"))
            mod <- coxph(Surv(time,event) ~ g, full)
            hr <- exp(mod$coefficients)[[1]]

            full <- rbind(n1,n3)
            full$g <- factor(full$g, levels=c("Control", "Exp"))
            mod <- coxph(Surv(time,event) ~ g, full)
            p <- summary(mod)$coefficients[, 5]
            #df <- rbind(df, data.frame(sample.size=ss, effect.size=hr, length=yr, p=p))
            if (is.na(p)) { p <- 1.0 }
            if (is.na(hr)) { hr <- 1.0 }
            data.frame(sample.size=ss, effect.size=hr, length=yr, p=p)
        }
        df <- rbind(df, res)
    }
}

write.csv(df, "simultation.csv", row.names=F)
