# Online Applied Data Mining, IUB, 2017
# Case Study: Prediction Algae Blooms
# Chapter 4 : Model Evaluation and Selection
# Data Mining with R by Luis Torgo
# Modified by Hasan Kurban
#######################################################################################
#A. CALCULATE ERRORS
#######################################################################################
# Calculating error for the linear regression and regression tree models that we have trained
#final.lm: linear regression model that we trained earlier-- it can be found in models.R
lm.predictions.a1 <- predict(final.lm, clean.algae) # prediction
#rt.a1: decision tree that we trained earlier -- it can be found in models.R
rt.predictions.a1 <- predict(rt.a1, algae) # prediction
(mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[["a1"]]))) # mean absolute error (MAE)
(mae.a1.rt <- mean(abs(rt.predictions.a1 - algae[["a1"]]))) # mean absolute error (MAE)
(mse.a1.lm <- mean((lm.predictions.a1 - algae[["a1"]])^2)) # mean squared error (MSE)
(mse.a1.rt <- mean((rt.predictions.a1 - algae[["a1"]])^2)) # mean squared error (MSE)
(nmse.a1.lm <- mean((lm.predictions.a1-algae[['a1']])^2)/  #Normalized MSE (NMSE)
    mean((mean(algae[['a1']])-algae[['a1']])^2))
(nmse.a1.rt <- mean((rt.predictions.a1-algae[['a1']])^2)/ #Normalized MSE (NMSE)
    mean((mean(algae[['a1']])-algae[['a1']])^2))


################################################################################################
#B. OBSERVE THE ERROR WITH SCATTER PLOTS
################################################################################################
# visualizing error
require(ggplot2) #load the package
# putting the results in a data frame
dg <- data.frame(lm.a1=lm.predictions.a1,
                 rt.a1=rt.predictions.a1,
                 true.a1=algae[["a1"]])

#errors via scatter plot -- linear model
ggplot(dg,aes(x=lm.a1,y=true.a1)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Linear Model")

#use identify() to detect the worst predictions. You can click on the data points to select.
plot(lm.predictions.a1,algae[['a1']],main="Linear Model",
     xlab="Predictions",ylab="True Values")
abline(0,1,col="red")
algae[identify(lm.predictions.a1,algae[['a1']]),]
# This plot shows that the model predicts negative algae frequencies for some cases.
# This is not possible. We will use this information to improve the model
sensible.lm.predictions.a1 <- ifelse(lm.predictions.a1 < 0, 0, lm.predictions.a1)
(mae.a1.lm <- mean(abs(lm.predictions.a1 - algae[["a1"]])))
(smae.a1.lm <- mean(abs(sensible.lm.predictions.a1 - algae[["a1"]])))

#errors via scatter plot -- regression tree
ggplot(dg,aes(x=rt.a1,y=true.a1)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Regression Tree")

#Both models shows poor performance. If they make correct predictions for all cases
# all the points in the plots should lie on the red line.
################################################################################################
#C. PERFORMANCE ESTIMATION FOR UNSEEN DATA -- Cross Validation etc.
################################################################################################
#install the library
install.packages("performanceEstimation")
library(performanceEstimation)
res <- performanceEstimation(
  #predictive task to compare: one linear model against 3 regression trees
  PredTask(a1 ~ ., algae[, 1:12], "a1"),
  # knnImp to replace missing values
  #onlyPos : to make negative predictions to 0. Since we know, thar is not possivle
  c(Workflow(learner="lm",pre="knnImp",post="onlyPos"),
    workflowVariants(learner="rpartXse",learner.pars=list(se=c(0,0.5,1)))),
  # NMSE: evaluation metric, 5 repetitions of 10-fold cross-validaton
  EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10)) 
)

summary(res) #Observe NSME. trees performed better

plot(res) # plot the results

getWorkflow("rpartXse.v1", res) # to get the parameters of any model
################################################################################################
#D. PERFORMANCE ESTIMATION FOR ALL SEVEN PREDICTION TASK
################################################################################################
#similar experiment  for all seven prediction tasks
DSs <- sapply(names(algae)[12:18],
              function(x,names.attrs) { 
                f <- as.formula(paste(x, "~ ."))
                PredTask(f, algae[,c(names.attrs,x)], x, copy=TRUE) 
              },
              names(algae)[1:11])
res.all <- performanceEstimation(
  DSs,
  c(Workflow(learner="lm", pre="knnImp", post="onlyPos"),
    workflowVariants(learner="rpartXse", learner.pars=list(se=c(0,0.5,1)))),
  EstimationTask(metrics="nmse" ,method=CV(nReps=5, nFolds=10)))

plot(res.all)

topPerformers(res.all)
################################################################################################
# E. FITTING ENSEMBLE MODELS (Random Forests) TO ALGAE DATA  
################################################################################################
install.packages("randomForest")
library(randomForest)
res.all <- performanceEstimation(
  DSs,
  c(Workflow(learner="lm", pre="knnImp",post="onlyPos"),
    workflowVariants(learner="rpartXse",
                     learner.pars=list(se=c(0,0.5,1))),
    workflowVariants(learner="randomForest", pre="knnImp",
                     learner.pars=list(ntree=c(200,500,700)))),
  EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10)))

# observe the results, top: the number of workflows -- returns the top three model
# for each prediction
rankWorkflows(res.all, top=3) 

#to check if the results are statistaclly significant
p <- pairedComparisons(res.all,baseline="randomForest.v3")
p$nmse$F.test
p$nmse$BonferroniDunn.test
CDdiagram.BD(p)