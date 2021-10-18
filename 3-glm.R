#!/usr/bin/env Rscript
library(glmnet)
library(PRROC)
library(ggplot2)
require(doMC)

options(warn=1)
registerDoMC(cores = 20)

set.seed(137)

dat <- read.csv("data/fields.tsv", sep = "\t", header=T)
dat$eye <- factor(dat$eye)
dat$gender <- factor(dat$gender)
for (yr in seq(1.0, 5.0, 0.5)) {
    print(paste("Training model for year:", yr))
    sdat <- data.frame(dat)
    sdat$status[sdat$time > yr] <- 0
    sdat$time[sdat$time > yr] <- yr
    trainData <- sdat[sdat$grp == "train", ]
    testData <- sdat[sdat$grp == "test", ]
    foldseq <- trainData$foldid

    trainy = subset(trainData, select=c(status, time))
    testy = subset(testData, select=c(status, time))

    trainData <- subset(trainData, select = -c(status,grp,foldid,time) )
    testData <- subset(testData, select = -c(status,grp,foldid,time) )

    x = model.matrix(~.,data=trainData)

    cvfit = cv.glmnet(x, y=as.matrix(trainy), alpha=0, lambda=10^(seq(5,-5,-0.05)), foldid = foldseq, parallel = T, relax=F, family="cox", type.measure = "C")
    saveRDS(cvfit, file=paste("data/finalmodel-", yr, ".rds", sep=""))
    pdf(paste("figures/glm-", yr,".pdf", sep=""))
    plot(cvfit)
    dev.off()

    params <- coef(cvfit,s=cvfit$lambda.min)
    summ <- summary(params)
    summ <- data.frame(variable = rownames(params)[summ$i], coeff = summ$x)
    write.csv(summ, paste("data/coef-", yr, ".csv", sep=""))

    outdf <- data.frame(prob = predict(cvfit,newx = x, s=cvfit$lambda.min, type="response"))
    p <- ggplot(outdf, aes(X1)) + geom_histogram(bins=100)
    ggsave(paste("figures/glm-prob-", yr, ".pdf", sep=""), p)
    cutoff <- quantile(outdf$X1)[[4]]

    x_test <- model.matrix(~.,data = testData)

    outdf <- data.frame(prob = predict(cvfit,newx = x_test, s=cvfit$lambda.min, type="response"), status = testy$status, time = testy$time)
    outdf$cutoff <- cutoff
    outdf$include <- 0
    outdf$include[outdf$X1 >= cutoff] <- 1
    write.csv(outdf, paste("data/model-", yr,".csv", sep=""), row.names=F)
}
