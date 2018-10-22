##Homework 2.3##

## Using the R function - filter(mydata, !complete.cases(mydata) ) - returns the entire dataset as NA. 

install.packages("DMwR2")
install.packages("rpart.plot")
install.packages("performanceEstimation")
library(DMwR2)
library(rpart.plot)

rndSample <- sample(1:nrow(mydata), 900)
mydata.training <- mydata[rndSample,]
mydata.testing <- mydata[-rndSample,]
