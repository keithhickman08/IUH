##Homework 2.3##
##Problem 3##

set.seed(1234)
View(mydata.training)
View(mydata.testing)

ct.test1 <- rpartXse(Purchase ~ ., mydata.training, se=0)

prp(ct.test1, type=0, extra=101)

summary(ct.test1)
