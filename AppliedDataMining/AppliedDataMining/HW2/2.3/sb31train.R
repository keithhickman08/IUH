##Homework 2.3##
##Problem 3##

install.packages("DMwR2")
install.packages("rpart.plot")
install.packages("performanceEstimation")

library(DMwR2)
library(rpart.plot)
library(performanceEstimation)

set.seed(1234)
mydata.training <- mydata[rndSample,]
View(mydata.training)

ct.train1 <- rpartXse(Purchase ~ ., mydata.training, se=0)
ct.train2 <- rpartXse(Purchase ~ ., mydata.training, se=.5)
ct.train3 <- rpartXse(Purchase ~ ., mydata.training, se=1)

prp(ct.train1, type=0, extra=101)
prp(ct.train2, type=0, extra=101)
prp(ct.train3, type=0, extra=101)

summary(ct.train1)
summary(ct.train2)
summary(ct.train3)


prp(ct.train1, type=0, extra=101)
prp(ct.train2, type=0, extra=101)
prp(ct.train3, type=0, extra=101)


## I tried this method, but kept getting an error:
##r<- performanceEstimation(
##  PredTask(purchasefac ~ ., mydata.training),
##  workflowVariants(learner="rpartXse",
##                   learner.pars=list(se=c(0, .5, 1))),
##  EstimationTask(metrics=c("mse","mae"),
##                 method=CV(nReps=3,nFolds=10))
##)
## summary(r)
