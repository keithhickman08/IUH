# Online Applied Data Mining, IUB, 2017
# Case Study: Prediction Algae Blooms
# Chapter 4 : Obtaining Prediction Models
# Data Mining with R by Luis Torgo
# Modified by Hasan Kurban
#######################################################################################
# 1. Multiple Linear Regression
#######################################################################################
data(algae, package="DMwR2") #load the data
algae <-  algae[-manyNAs(algae), ]  #remove rows with NAs where 20% variables are missing
clean.algae <- knnImputation(algae, k = 10)  # Fill in NAs using KNN, k: the number of nearest neighbours 
dim(clean.algae)

#training a linear model to predict a1 variable using variables 1-12
lm.a1 <- lm(a1 ~ ., data = clean.algae[, 1:12]) # lm() obtains a linear regressopn model
#results: residuals show the error ( have a mean zero and normal distribution). 
# R^2 shows th proportion of variance in the data that is explained by the model (degree of fit)
# Values near 1 are better (100% explained variance)
# F-statistic: to check if the variables are independent. 
# R is the confidence level at which we are sure to reject the null hypothesis.
# p-value shows how confident we are that the null hypothesis is not true.
#Coefficients: 
     #  1- Estimate: coefficient value for each variable
     #  2- Std. Error: an estimate of the variability of these coefficients.
     #  3- t value and Pr(> |t|): to statistically check the importance of each coefficient
summary(lm.a1) # to observe detail of the linear model 
plot(lm.a1) #plots to understand performance of the model
# to simplify the linear model use anova()
anova(lm.a1)
# it shows that season is the variable that least contributes to the reduction of the fitting error of the model
#remove it
lm2.a1 <- update(lm.a1, . ~ . - season) # update function to perform small changes to existing linear models
summary(lm2.a1) # observe the new results
anova(lm.a1,lm2.a1) # compare the two linear models that we have created
final.lm <- step(lm.a1) # optimize lm.a1 using AIC ( model search)
summary(final.lm) 
# the proportion of the variance explained by the model is not very interesting.
# it shows that fitting a linear model to algae data is not a good idea.
#######################################################################################
# 2. Regression Trees
#######################################################################################
# fit a regression tree to algae data set using rpart package
require(rpart) # load the package
data(algae, package="DMwR2") # load the data 
algae <- algae[-manyNAs(algae), ]  #remove rows with NAs where 20% variables are missing
#training a regression tree to predict a1 variable using variables 1-12
rt.a1 <- rpart(a1 ~ ., data = algae[, 1:12])
rt.a1
require(rpart.plot) # package for visualization of regression trees
?prp
prp(rt.a1,extra=101,box.col="orange",split.box.col="grey") # plot the tree
# produce a set of sub-trees of this tree and estimate their performance
#the tree returned by rpart() function is the last tree of this list
printcp(rt.a1) 
# cp is to optimize predictive accuracy and tree size 
rt2.a1 <- prune(rt.a1, cp = 0.08) # update thee tree with cp
rt2.a1
# rpartXse the function that combines prune.part(), which is for postpruning,
#and rpart(), which is for growing the trees, functions in a function
require(rpartXse) #book package
(rt.a1 <- rpartXse(a1 ~ ., data = algae[, 1:12]))
# interactive pruning
first.tree <- rpart(a1 ~ ., data = algae[, 1:12])
snip.rpart(first.tree, c(4, 7))  
plot(first.tree)
text(first.tree)
snip.rpart(first.tree)