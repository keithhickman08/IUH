#Tree-based Models
#Data Minnig with R Learning with case studies by Luis Targo
#Modified by Hasan Kurban - I590 Online Applied Data Mining
#Fall 2017

###########################################################################################
# rpart package: Implementation of tree-baed models in R. 
# There is another package for  tree based models in R-- party package
# rpartXse: the function that combines prune.part(), which is for postpruning,
#and rpart(), which is for growing the trees, functions in a function
#For more information about them
?rpart
?rpartXse 
install.packages("DMwR2")
library(DMwR2)
set.seed(1234)
data(iris) #load iris data set
names(iris) #name of the variables, Species is the class variable

## (1) Train decison trees and visualize them.
## Build two classification trees with different post-pruning criterions for the Iris data set
ct1 <- rpartXse(Species ~ ., iris) #default se =1: more aggresive pruning
ct1
ct2 <- rpartXse(Species ~ ., iris, se=0) # se= 0: less aggresive pruning
ct2

#install rpart.plot to visualize the trees 
?rpart.plot
install.packages("rpart.plot")
library(rpart.plot)

par( mfrow=c(1,2) ) # this is to divide the plots envinoment into two sections
?prp #this function combines plot.rpart amd text.rpart functions in one function
prp(ct1,type=0,extra=101) #type: type of plot, extra: to display extra info on nodes
prp(ct2,type=0,extra=101)
###########################################################################################
## (2) Complete example for tree based models: Train a decision tree and use it to
## predict test data -- new data points 

# In this part, we first train a decison tree, and then use it to classify
# the new data points -- test data
set.seed(1234)  
# Creating training  and testing  data sets: Randomly picking 100 data points from Iris data set
# as training data and the rest of 50 data points will be used as test data.
rndSample <- sample(1:nrow(iris),100)
tr <- iris[rndSample, ]  # training data: 100 data points
ts <- iris[-rndSample, ] #  testing data: 50 data points
ct <- rpartXse(Species ~ ., tr, se=0.5) # training: use only training data to build a decision tree
ps1 <- predict(ct, ts)  #testing: use the decision tree to classify test data
ps1 # estimated probabilities of each class for each test data point
#type = class: predicted classes with the names instead of prob.
ps2 <- predict(ct, ts, type="class") 
ps2 # numbers show the location of the data points in the data
(cm <- table(ps2, ts$Species)) #confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm))  # the error rate is 6%