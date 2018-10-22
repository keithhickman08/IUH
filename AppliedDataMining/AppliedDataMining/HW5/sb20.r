library(performanceEstimation)
res <- performanceEstimation(PredTask(mpg ~ ., carsclean, "mpg"),
     c(Workflow(learner="lm",pre="knnImp",post="onlyPos"),
     workflowVariants(learner="rpartXse",learner.pars=list(se=c(0,0.5,1)))),
     EstimationTask(metrics="nmse",method=CV(nReps=5,nFolds=10)))

res2 <- performanceEstimation(PredTask(mpg ~ ., carsclean, "mpg"),
    c(Workflow(learner="lm",pre="knnImp",post="onlyPos"),
    workflowVariants(learner="rpartXse",learner.pars=list(se=c(0,0.5,1)))),
    EstimationTask(metrics="mse",method=CV(nReps=3,nFolds=5)))

summary(res)
summary(res2)
