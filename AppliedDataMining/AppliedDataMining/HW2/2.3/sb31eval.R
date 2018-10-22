##Homework 2.3##
##Problem 3##
##Evaluate the Results##

ct.test1 <- rpartXse(Purchase ~ ., mydata.testing, se=.0)

summary(ct.test1)

ps1 <- predict(ct.train1, mydata.testing)
ps1

ps2 <- predict(ct.train1, mydata.testing, type="class")
ps2

cm <- table(ps2, mydata.testing$Purchase)
cm
100*(1-sum(diag(cm))/sum(cm))