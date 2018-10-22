#Homework 4
# Problem 6

#H20: An open source, scalable machine learning platform with interfaces to many languages including R: h2o package
#you can define many things i.e., memory while starting an H20 instance with h2o.init()
#For example; h2o.init(max_mem_size = "5g")
#more information about h2o and h2o.init
?h2o 
?h2o.init
#instal the package
install.packages("h2o")
library(h2o)
h2oInstance <- h2o.init(ip="localhost")  # start H2O instance locally

set.seed(1234)

rndSample <- sample(1:nrow(mydata), 900)
mydata.trainingH  <- as.h2o(mydata[rndSample, ],"mydata.trainingH")
mydata.testingH <- as.h2o(mydata[-rndSample, ],"mydata.testingH")

?h2o.deeplearning
# y is the class variable number, x: the rest of the variables, training_frame: training data
mdl <- h2o.deeplearning(x=2:18, y=1, training_frame=mydata.trainingH)
mdl
#classify the test data using the trained DLNN
#mdl: is the trained DLNN, tsH: test data
preds <- h2o.predict(mdl,tsH)[,"predict"]
preds

(cm <- table(as.vector(preds), as.vector(tsH$Species))) #confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm)) #the error rate 