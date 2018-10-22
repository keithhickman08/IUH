#Artificial Neural Networks and Deep Learning
#Data Minnig with R Learning with case studies by Luis Targo
#Modified by Hasan Kurban - I590 Online Applied Data Mining
#Fall 2017
###########################################################################################
#Artificial Neural Networks (ANN) package in R: nnet
###########################################################################################
## (1) ANN on Iris data set: Classification

#install the package
install.packages("nnet")
library(nnet)
?nnet # more information about the function
data(iris)

set.seed(1234)
# Creating training  and testing  data sets: Randomly picking 100 data points from Iris data set
# as training data and the rest of 50 data points will be used as test data.
rndSample <- sample(1:nrow(iris), 100)
tr <- iris[rndSample, ] #training data: randomly picked 100 points from iris data
ts <- iris[-rndSample, ] # test data: 50 data points

#size: number of neurons in the hidden layer
n <- nnet(Species ~ ., tr, size=6 ,trace=FALSE, maxit=1000) # train an ANN over iris data set
n
ps <- predict(n, ts, type="class")  #  classify test data using the trained ANN
ps
(cm <- table(ps, ts$Species)) # confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # the error rate is 4%: 2 data points are misclassified, out of 50 points
###########################################################################################
## (2) ANN on Iris data set: Regression

data(Boston,package='MASS')
dim(Boston) #size of the data : 506 data points in 14 dimensions
names(Boston) # name of the variables
head(Boston$medv) # train an ANN model to predict "medv" values 



# Creating training  and testing  data sets: Randomly picking 354 data points from Boston data set
# as training data and the rest of 152 data points will be used as test data.
set.seed(1234)
sp <- sample(1:nrow(Boston),354) 
tr <- Boston[sp,] #training data: randomly picked 354 points from Boston data set
ts <- Boston[-sp,] #test data: 152 data points

# train an ANN on the training data
nr <- nnet(medv ~ ., tr, linout=TRUE, trace=FALSE, size=6, decay=0.01, maxit=2000)
# predict "medv" values using the trained ANN
psnr <- predict(nr, ts) # confusion matrix for evaluation
mean(abs(psnr-ts$medv)) #the error is 3.028373

###########################################################################################
## (3) Visualization of ANNs using Boston data set
install.packages("ggplot2")
library(ggplot2)
install.packages("NeuralNetTools")
library(NeuralNetTools)
## Feature importance (left graph)
garson(nr) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
## Network diagram (rigth graph)
plotnet(nr) 