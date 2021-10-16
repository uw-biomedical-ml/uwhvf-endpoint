#!/usr/bin/env Rscript

library(ggplot2)

dat <- read.csv("power.csv")
sdat <- dat[which(dat$simn > 100),]

summary(sdat)

pow80 <- NULL
for (hr in sort(unique(sdat$hazard.ratio))) {
    if (hr <= 0.40) { next }
    for (l in sort(unique(sdat$length))) {
        tdat <- sdat[which(sdat$length == l & sdat$hazard.ratio == hr),]
        if (nrow(tdat) < 5) {
            next
        }
        if (max(tdat$power) <= 80.0 | min(tdat$power) >= 80) {
            next
        }
        #inter <- spline(tdat$power, tdat$sample.size, xmin=10, xmax=90, xout=seq(10,90,10))
        inter <- approx(tdat$power, tdat$sample.size, xout=seq(10,90,10))
        if (inter$y[8] < 0) {
            print(inter)
        } else {
            pow80 <- rbind(pow80, data.frame(hazard.ratio = hr, length = l, sample.size=inter$y[8]))
        }
    }
}
print(pow80)

pow80$length <- factor(pow80$length)
p <- ggplot(pow80, aes(hazard.ratio, sample.size, color=length, group=length)) + geom_point() + geom_smooth() + scale_color_brewer(palette="Set1")
ggsave("pow80.png", p)

quit()

p <- ggplot(sdat, aes(hazard.ratio, power, color=log(simn))) + geom_point() + facet_grid(sample.size ~ length)

ggsave("power.png", p, width=12, height=6)



quit()
dat <- read.csv("simultation.csv")
summary(dat)

p <- ggplot(dat, aes(effect.size, p)) + geom_point(alpha=0.2) + scale_color_brewer(palette="Set1") + facet_grid(sample.size ~ length)

ggsave("raw.png", p, width=12, height=6)

