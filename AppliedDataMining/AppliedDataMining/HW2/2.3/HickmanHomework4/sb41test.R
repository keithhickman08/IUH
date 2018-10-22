#Homework 4
#Problem 4
#Support Vector Machines

??e1071
install.packages("e1071")
library(e1071)

set.seed(1234)

s2test <- svm(Purchase ~ ., mydata.test, cost=10, kernel="polynomial", degree=3)
ps2test <- predict(s2test, mydata.test)
(cm2test <- table(ps2test, mydata.test$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm2test))/sum(cm2test))  
