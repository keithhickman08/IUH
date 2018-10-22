#Homework 4
#Problem 4
#Support Vector Machines

??e1071
install.packages("e1071")
library(e1071)

set.seed(1234)
# Creating training  and testing  data sets: Randomly picking 100 data points from Iris data set
# as training data and the rest of 50 data points will be used as test data.
rndSample <- sample(1:nrow(mydata), 900)
mydata.training <- mydata[rndSample,] 
mydata.test <- mydata[-rndSample, ]

s <- svm(Purchase ~ ., mydata.training)
ps <- predict(s, mydata.training)
(cm <- table(ps, mydata.training$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # the error rate is 14%


# Adjusting some of the parameters of SVM
# In this example we are changing radial kernel to polynomial kernel
# and changing cost from 1 to 10
#cost argument: to specify the cost of a violation to the margin.
#if the cost is small, the margin is large  -- more support vectors violating the margin
#if the cost is large, the margin is narrow -- less support vectors violating the margin
# Kernels are used to map linearly non-separable data to higher dimenisonal space so that
# the data can be linearly seperable.

s2 <- svm(Purchase ~ ., mydata.training, cost=10, kernel="polynomial", degree=3)
ps2 <- predict(s2, mydata.training)
(cm2 <- table(ps2, mydata.training$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm2))/sum(cm2))  

# the error rate for a cost of 10 is 14%
# the error rate for a cost of 20 is also 14%
# modifying the degree parameter does not substantially improve the error rate. 