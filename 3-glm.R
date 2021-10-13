#!/usr/bin/env Rscript
library(glmnet)

dat <- read.csv("fields.tsv", sep = "\t", header=F)
trainDataIndex <- sample(1:nrow(dat), 0.7*nrow(dat))
trainData <- dat[trainDataIndex, ]
testData <- dat[-trainDataIndex, ]
summary(dat)

x = model.matrix(V55 ~.,data=trainData)
cvfit = cv.glmnet(x, y=as.factor(trainData$V55), alpha=1, family="binomial",type.measure = "class")

pdf("glm.pdf")
plot(cvfit)
dev.off()
