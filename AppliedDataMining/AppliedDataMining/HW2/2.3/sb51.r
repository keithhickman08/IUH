#Homework 4
# Problem 5

install.packages("nnet")
library(nnet)

set.seed(1234)

rndSample <- sample(1:nrow(mydata), 900)
mydata.training <- mydata[rndSample, ] 
mydata.test <- mydata[-rndSample, ] 

#First ANN
n1 <- nnet(Purchase ~ ., mydata.training, size=10 ,trace=FALSE, maxit=1000)
n1

#Second ANN
n2 <- nnet(Purchase ~ ., mydata.training, size=50 ,trace=FALSE, maxit=1000)
n2

ps1 <- predict(n1, mydata.test, type="class")
ps1

ps2 <- predict(n2, mydata.test, type="class")
ps2

(cm1 <- table(ps1, mydata.test$Purchase)) # confusion matrix for evaluation
100*(1-sum(diag(cm1))/sum(cm1))  # the error rate is 4%: 2 data points are misclassified, out of 50 points

(cm2 <- table(ps1, mydata.test$Purchase))
100*(1-sum(diag(cm2))/sum(cm2))


## (3) Visualization of ANNs using Boston data set
install.packages("ggplot2")
library(ggplot2)
install.packages("NeuralNetTools")
library(NeuralNetTools)
## Feature importance (left graph)
garson(nr) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
## Network diagram (rigth graph)
plotnet(nr) 