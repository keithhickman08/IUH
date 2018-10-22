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
ps <- predict(s, mydata.test)
(cm <- table(ps, mydata.test$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # the error rate is 18%


# Adjusting some of the parameters of SVM
# In this example we are changing radial kernel to polynomial kernel
# and changing cost from 1 to 10
#cost argument: to specify the cost of a violation to the margin.
#if the cost is small, the margin is large  -- more support vectors violating the margin
#if the cost is large, the margin is narrow -- less support vectors violating the margin
# Kernels are used to map linearly non-separable data to higher dimenisonal space so that
# the data can be linearly seperable.

s2 <- svm(Purchase ~ ., mydata.training, cost=10, kernel="polynomial", degree=3)
ps2 <- predict(s2, mydata.test)
(cm2 <- table(ps2, mydata.test$Purchase)) #confusion matrix for evaluation
100*(1-sum(diag(cm2))/sum(cm2))  

# the error rate for a cost of 10 is 17%
# the error rate for a cost of 20 is also 17%
# modifying the degrees does not substantially improve the error rate. 
###########################################################################################


## (2) SVM on Boston data set : Regression
data(Boston,package='MASS')
dim(Boston) #size of the data : 506 data points in 14 dimensions
names(Boston) # name of the variables
head(Boston$medv) # train an SVM model to predict "medv" values 

set.seed(1234)
# Creating training  and testing  data sets: Randomly picking 354 data points from Boston data set
# as training data and the rest of 152 data points will be used as test data.
sp <- sample(1:nrow(Boston),354)
tr <- Boston[sp,] #training data: randomly picked 354 points from Boston data set
ts <- Boston[-sp,] # test data: 152 data points
s1 <- svm(medv ~ ., tr) # train an SVM on the training data
ps1 <- predict(s1, ts)  # predict "medv" values 
ps1
mean(abs(ps1-ts$medv)) #error: 2.769211

# Adjusting some of the parameters of SVM
#changing cost from 1 to 10 -- narrow margin
#gamma: to specify value of gamma for the radial kernel function
?svm
s2 <- svm(medv ~ ., tr, kernel="radial", cost=10, epsilon=0.02, gamma=0.01)#training
ps2 <- predict(s2, ts) #testing
mean(abs(ps2-ts$medv))  # error 2.400234
# with the new parameters, we observe better performance. ( the new error is lower)