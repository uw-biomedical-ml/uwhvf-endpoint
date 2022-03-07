#!/usr/bin/env Rscript
library(glmnet)
library(ggplot2)
require(doMC)

options(warn=1)
registerDoMC(cores = 20)

# set seed for reproducibility
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

    trainData <- subset(trainData, select = -c(status,grp,foldid,time,startmd) )
    testData <- subset(testData, select = -c(status,grp,foldid,time,startmd) )

    x = model.matrix(~.,data=trainData)

    # lambda range sequence found by grid search, alpha of 0 also found by grid search 
    df = NULL
    for (alpha in seq(0,1,length=100)^4) {
        error <- NULL
        for (rep in seq(0,1)) {
            cvfit = cv.glmnet(x, y=as.matrix(trainy), alpha=alpha, lambda=10^(seq(5,-5,-0.05)), foldid = foldseq, parallel = T, relax=T, family="cox", type.measure = "C")
            error <- c(error, cvfit$cvm)
        }
        print(error)
        print(c(alpha, mean(error)))
        error <- mean(error)
        df <- rbind(df, data.frame(alpha=alpha, error=error))
        print(c(alpha,error))
    }
    print(df)
    p <- ggplot(df, aes(alpha, error)) + geom_line() + geom_point()
    ggsave(paste("figures/glmnet.alpha-",yr,".pdf", sep=""), p)
    df <- df[order(-df$error),]
    finalalpha <- df[1,]$alpha

    print(finalalpha)

    cvfit = cv.glmnet(x, y=as.matrix(trainy), alpha=finalalpha, lambda=10^(seq(5,-5,-0.05)), foldid = foldseq, parallel = T, relax=T, family="cox", type.measure = "C")
    saveRDS(cvfit, file=paste("data/finalmodel-", yr, ".rds", sep=""))
    pdf(paste("figures/glm-", yr,".pdf", sep=""))
    plot(cvfit)
    dev.off()

    # save model coefficients for visualization of field
    params <- coef(cvfit,s=cvfit$lambda.min)
    summ <- summary(params)
    summ <- data.frame(variable = rownames(params)[summ$i], coeff = summ$x)
    write.csv(summ, paste("data/coef-", yr, ".csv", sep=""))

    # Run trained model over training data to get distribution of risks
    outdf <- data.frame(prob = predict(cvfit,newx = x, s=cvfit$lambda.min, type="response"))
    p <- ggplot(outdf, aes(X1)) + geom_histogram(bins=100)
    ggsave(paste("figures/glm-prob-", yr, ".pdf", sep=""), p)
    # Cutoff determined at the 75th perccentile 
    cutoff <- quantile(outdf$X1)[[4]]

    # Run inference over the heldout test set 
    x_test <- model.matrix(~.,data = testData)

    outdf <- data.frame(prob = predict(cvfit,newx = x_test, s=cvfit$lambda.min, type="response"), status = testy$status, time = testy$time)
    outdf$cutoff <- cutoff
    outdf$include <- 0
    outdf$include[outdf$X1 >= cutoff] <- 1
    write.csv(outdf, paste("data/model-", yr,".csv", sep=""), row.names=F)
}
