#!/usr/bin/env Rscript
library(glmnet)
library(PRROC)

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

grp0 <- lasso_prob[testData$V55 == 0,]
grp1 <- lasso_prob[testData$V55 == 1,]
print(mean(grp0))
print(mean(grp1))
roc<-roc.curve(scores.class0 = grp1, scores.class1 = grp0, curve=T)
pr<-pr.curve(scores.class0 = grp1, scores.class1 = grp0, curve=T)

pdf("glm.auc.pdf")
plot(roc)
dev.off()


pdf("glm.pr.pdf")
plot(pr)
dev.off()

print(roc)
print(pr)
quit()

pred <- prediction(lasso_prob, testData$V55)
perf <- performance(pred,"tpr","fpr")
print(performance(pred,"auc")) # shows calculated AUC for model
pdf("glm.auc.pdf")
plot(perf, col=rainbow(10))
dev.off()


lasso_predict <- rep("0",nrow(testData))
lasso_predict[lasso_prob>.5] <- "1"
table(pred=lasso_predict,true=testData$V55)

mean(lasso_predict==testData$V55)
