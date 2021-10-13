#!/usr/bin/env Rscript
library(glmnet)
library(ROCR)

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

lambda_1se <- cvfit$lambda.1se
#print(coef(cvfit,s=lambda_1se))

x_test <- model.matrix(V55~.,data = testData)

lasso_prob <- predict(cvfit,newx = x_test,s=lambda_1se,type="response")
pred <- prediction(lasso_prob, testData$V55)
perf <- performance(pred,"tpr","fpr")
pdf("glm.auc.pdf")
performance(pred,"auc") # shows calculated AUC for model
plot(perf, col=rainbow(10))
dev.off()


lasso_predict <- rep("0",nrow(testData))
lasso_predict[lasso_prob>.5] <- "1"
table(pred=lasso_predict,true=testData$V55)

mean(lasso_predict==testData$V55)
