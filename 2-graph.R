#!/usr/bin/env Rscript

library(survminer)
library(survival)

dat <- read.csv("ptlvl.tsv", sep="\t")
#dat <- dat[which(dat$age > 18),]

dat$eye <- factor(dat$eye)
dat$gender <- factor(dat$gender)

dat$agecat <- NULL
dat$agecat[dat$age < 50] <- "< 50"
dat$agecat[dat$age >= 50 & dat$age < 60] <- "50 - 60"
dat$agecat[dat$age >= 60 & dat$age < 70] <- "60 - 70"
dat$agecat[dat$age >= 70 & dat$age < 80] <- "70 - 80"
dat$agecat[dat$age >= 80 ] <- "> 80"

dat$agecat <- factor(dat$agecat, levels=c("< 50", "50 - 60", "60 - 70", "70 - 80", "> 80"))


dat$startmdcat <- NULL
dat$startmdcat[dat$startmd > -2.5] <- "> -2.5"
dat$startmdcat[dat$startmd > -5.0 & dat$startmd <= -2.5] <- "-5 to -2.5"
dat$startmdcat[dat$startmd > -10.0 & dat$startmd <= -5] <- "-10 to -5"
dat$startmdcat[dat$startmd > -15.0 & dat$startmd <= -10] <- "-15 to -10"
dat$startmdcat[dat$startmd <= -15] <- "< -15"

dat$startmdcat <- factor(dat$startmdcat, levels=c("> -2.5", "-5 to -2.5", "-10 to -5", "-15 to -10", "< -15"))

dat$yearcat <- NULL
dat$yearcat[dat$year < 2005] <- "< 2005"
dat$yearcat[dat$year >= 2005 & dat$year < 2010] <- "2005 - 2010"
dat$yearcat[dat$year >= 2010 & dat$year < 2015] <- "2010 - 2015"
dat$yearcat[dat$year >= 2015] <- "> 2015"

dat$yearcat <- factor(dat$yearcat, levels=c("< 2005", "2005 - 2010", "2010 - 2015", "> 2015"))

summary(dat)

pdf("full.pdf")
fit <- survfit(Surv(time, event) ~ 1, data = dat)
ggsurvplot(
  fit, 
  data = dat, 
  size = 1,                 # change line size
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  palette = "jco",
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  xlim = c(0, 5),
  break.x.by = 1,
  ggtheme = theme_bw()      # Change ggplot2 theme
)
dev.off()


dat$Age <- dat$agecat

pdf("agecat.pdf")
fit <- survfit(Surv(time, event) ~ Age, data = dat)
ggsurvplot(
  fit, 
  data = dat, 
  size = 1,                 # change line size
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  palette = "jco",
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  xlim = c(0, 5),
  break.x.by = 1,
  ggtheme = theme_bw()      # Change ggplot2 theme
)
dev.off()

dat$MD <- dat$startmdcat

pdf("mdcat.pdf")
fit <- survfit(Surv(time, event) ~ MD, data = dat)
ggsurvplot(
  fit, 
  data = dat, 
  size = 1,                 # change line size
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  palette = "jco",
  risk.table = TRUE,        # Add risk table
  risk.table.col = "strata",# Risk table color by groups
  risk.table.height = 0.25, # Useful to change when you have multiple groups
  xlim = c(0, 5),
  break.x.by = 1,
  ggtheme = theme_bw()      # Change ggplot2 theme
)
dev.off()



pdf("facet.pdf")
fit <- survfit(Surv(time, event) ~ MD, data = dat)
ggsurvplot_facet(
  fit, 
  data = dat, 
  facet.by = "Age",
  size = 1,                 # change line size
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  palette = "jco",
  xlim = c(0, 5),
  break.x.by = 1,
  ggtheme = theme_bw()      # Change ggplot2 theme
)
dev.off()

dat$Year <- dat$yearcat

pdf("facet-year.pdf")
fit <- survfit(Surv(time, event) ~ Year, data = dat)
ggsurvplot_facet(
  fit, 
  data = dat, 
  facet.by = "MD",
  size = 1,                 # change line size
  conf.int = TRUE,          # Add confidence interval
  pval = TRUE,              # Add p-value
  palette = "jco",
  xlim = c(0, 5),
  break.x.by = 1,
  ggtheme = theme_bw()      # Change ggplot2 theme
)
dev.off()





