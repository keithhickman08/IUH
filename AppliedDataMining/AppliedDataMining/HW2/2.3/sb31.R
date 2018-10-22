##Homework 2.3##
##Problem 3##
##Fit and Visualize the Data##

rndSample <- sample(1:nrow(mydata), 900)
mydata.training <- mydata[rndSample,]
mydata.testing <- mydata[-rndSample,]

ct.train1 <- rpartXse(Purchase ~ ., mydata.training, se=.0)
ct.train2 <- rpartXse(Purchase ~ ., mydata.training, se=.5)
ct.train3 <- rpartXse(Purchase ~ ., mydata.training, se=1)

prp(ct.train1, type=0, extra=101)
prp(ct.train2, type=0, extra=101)
prp(ct.train3, type=0, extra=101)

summary(ct.train1)
summary(ct.train2)
summary(ct.train3)

## CM for ct.train1
ps11 <- predict(ct.train1, mydata.testing)
ps11

ps12 <- predict(ct.train1, mydata.testing, type="class")
ps12

cm <- table(ps12, mydata.testing$Purchase)
cm
100*(1-sum(diag(cm))/sum(cm))

## CM for ct.train2
ps21 <- predict(ct.train1, mydata.testing)
ps21

ps22 <- predict(ct.train1, mydata.testing, type="class")
ps22

cm <- table(ps22, mydata.testing$Purchase)
cm
100*(1-sum(diag(cm))/sum(cm))

## CM for ct.train3
ps31 <- predict(ct.train3, mydata.testing)
ps31

ps32 <- predict(ct.train3, mydata.testing, type="class")
ps32

cm <- table(ps32, mydata.testing$Purchase)
cm
100*(1-sum(diag(cm))/sum(cm))

##Error rate on SE = 0 is 22%
##Error rate on SE = .5 is 22.94%
##Error rate on SE = 1 is 26.47%
## The greater the SE parameter, the greater the error. 