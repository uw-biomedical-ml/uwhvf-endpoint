#!/usr/bin/env Rscript
library(caret)
library(PRROC)
library(ggplot2)

dat <- read.csv("fields.tsv", sep = "\t", header=F)
dat$V55[dat$V55 == 0] <- "stable"
dat$V55[dat$V55 == 1] <- "progress"
dat$V55 <- factor(dat$V55, levels=c("stable","progress"))
summary(dat$V55)
trainDataIndex <- sample(1:nrow(dat), 0.7*nrow(dat))
trainData <- dat[trainDataIndex, ]
testData <- dat[-trainDataIndex, ]

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           classProbs = T,
                           summaryFunction = twoClassSummary,
                           ## repeated ten times
                           repeats = 10)

gbmFit1 <- train(V55 ~ ., data = trainData, 
                 method = "gbm",
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = F,
                 metric = "ROC")
print(gbmFit1)
pdf("gbm.1.pdf")
trellis.par.set(caretTheme())
plot(gbmFit1, metric = "ROC")
dev.off()

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)
nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(V55 ~ ., data = trainData,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models
                 ## to evaluate:
                 tuneGrid = gbmGrid,
                 metric = "ROC")
print(gbmFit2)
pdf("gbm.2.pdf")
trellis.par.set(caretTheme())
plot(gbmFit2, metric = "ROC")
dev.off()


