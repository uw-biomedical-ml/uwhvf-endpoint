#!/usr/bin/env Rscript
library(glmnet)
library(PRROC)
library(ggplot2)
require(doMC)

options(warn=1)
registerDoMC(cores = 20)

set.seed(137)

dat <- read.csv("fields.tsv", sep = "\t", header=T)
dat$eye <- factor(dat$eye)
dat$gender <- factor(dat$gender)
for (yr in seq(1.0, 5.0, 0.5)) {
#for (yr in c(1.5)) {
    print(yr)
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
    print(cvfit$cvm)
    saveRDS(cvfit, file=paste("finalmodel-", yr, ".rds", sep=""))
    pdf(paste("glm-", yr,".pdf", sep=""))
    plot(cvfit)
    dev.off()
    outdf <- data.frame(prob = predict(cvfit,newx = x, s=cvfit$lambda.min, type="response"))
    print(head(outdf))
    p <- ggplot(outdf, aes(X1)) + geom_histogram(bins=100)
    ggsave(paste("glm-prob-", yr, ".pdf", sep=""), p)
    print(quantile(outdf$X1))
    print(summary(outdf$X1))
    cutoff <- quantile(outdf$X1)[[4]]

    x_test <- model.matrix(~.,data = testData)

    outdf <- data.frame(prob = predict(cvfit,newx = x_test, s=cvfit$lambda.min, type="response"), status = testy$status, time = testy$time)
    outdf$cutoff <- cutoff
    outdf$include <- 0
    outdf$include[outdf$X1 >= cutoff] <- 1
    write.csv(outdf, paste("samplesize/model-", yr,".csv", sep=""), row.names=F)
}
quit()

    # grid search over alpha with fixed 10-CV folds precalculated to be pt-level splits
    #
    #df = NULL
    #for (alpha in seq(0,1,length=100)^4) {
    #    error <- NULL
    #    for (rep in seq(0,1)) {
    #        cvfit = cv.glmnet(x, y=as.matrix(trainy), alpha=alpha, foldid = foldseq, parallel = T, relax=T, family="cox", type.measure = "C")
    #        error <- c(error, cvfit$cvm)
    #    }
    #    error <- mean(error)
    #    df <- rbind(df, data.frame(alpha=alpha, error=error))
    #    print(c(alpha,error))
    #}
    #print(df)
    #p <- ggplot(df, aes(alpha, error)) + geom_line() + geom_point()
    #ggsave("glmnet.alpha.pdf", p)
    #df <- df[order(-df$error),]
    #finalalpha <- df[1,]$alpha
    #
    #print(finalalpha)
    #
     pdf("glm.pdf")
    plot(cvfit)
    dev.off()

    lambda_min <- cvfit$lambda.min
    params <- coef(cvfit,s=lambda_min)
    params <- as.data.frame(summary(params))
    write.csv(params, "coef.csv")

    x_test <- model.matrix(~.,data = testData)

    outdf <- data.frame(prob = predict(cvfit,newx = x_test,s=lambda_min,type="response"), status = testy$status, time = testy$time)
    print(head(outdf))
    write.csv(outdf, "bootstrap-roc-prc/model.csv", row.names=F)


    grp0 <- outdf$s1[outdf$truth == 0]
    grp1 <- outdf$s1[outdf$truth == 1]
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

    lambda_1se <- cvfit$lambda.1se
    params <- coef(cvfit,s=lambda_1se)
    params <- as.data.frame(summary(params))
    write.csv(params, "coef.csv")

    x_test <- model.matrix(V56~.,data = testData)
    testTruth <- testData$V56
    print(nrow(testData))
    print(nrow(testData))
    print(testTruth)

    outdf <- data.frame(prob = predict(cvfit,newx = x_test,s=lambda_1se,type="response"), truth = testTruth)
    print(head(outdf))
    write.csv(outdf, "bootstrap-roc-prc/model.csv", row.names=F)


    grp0 <- outdf$s1[outdf$truth == 0]
    grp1 <- outdf$s1[outdf$truth == 1]
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

pred <- prediction(lasso_prob, testData$V57)
perf <- performance(pred,"tpr","fpr")
print(performance(pred,"auc")) # shows calculated AUC for model
pdf("glm.auc.pdf")
plot(perf, col=rainbow(10))
dev.off()


lasso_predict <- rep("0",nrow(testData))
lasso_predict[lasso_prob>.5] <- "1"
table(pred=lasso_predict,true=testData$V57)

mean(lasso_predict==testData$V57)
