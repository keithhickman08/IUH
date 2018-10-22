#Deep Learning Neural Networks (DLNNs)
#Data Minnig with R Learning with case studies by Luis Targo
#Modified by Hasan Kurban - I590 Online Applied Data Mining
#Fall 2017
###########################################################################################
#Deep Learning Neural Networks packages in R: h2o, mxnet, darch, deepnet
###########################################################################################
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

## (1) DLNN on Iris data set: Classification
data(iris)
set.seed(1234)
# Creating training  and testing  data sets: Randomly picking 100 data points from Iris data set
# as training data and the rest of 50 data points will be used as test data.
rndSample <- sample(1:nrow(iris), 100)
trH  <- as.h2o(iris[rndSample, ],"trH") #training data: randomly picked 100 points from iris data
tsH <- as.h2o(iris[-rndSample, ],"tsH") # test data: 50 data points
# train a deep feed-forward multi-layer ANN (DLNN) over iris data set
?h2o.deeplearning
# y is the class variable number, x: the rest of the variables, training_frame: training data
mdl <- h2o.deeplearning(x=1:4, y=5, training_frame=trH)
mdl
#classify the test data using the trained DLNN
#mdl: is the trained DLNN, tsH: test data
preds <- h2o.predict(mdl,tsH)[,"predict"]
preds

(cm <- table(as.vector(preds), as.vector(tsH$Species))) #confusion matrix for evaluation
100*(1-sum(diag(cm))/sum(cm)) #the error rate 
###########################################################################################
## (2) DLNN on Boston data set: Regression

h2oInstance <- h2o.init(ip="localhost") # start H2O instance locally
data(Boston,package="MASS")

# Creating training  and testing  data sets: Randomly picking 354 data points from Boston data set
# as training data and the rest of 152 data points will be used as test data.
rndSample <- sample(1:nrow(Boston), 354)
trH <- as.h2o(Boston[rndSample, ],"trH") #training data: randomly picked 354 points from Boston data set
tsH <- as.h2o(Boston[-rndSample, ],"tsH") # test data: 152 data points
#y: class variable number, x: the rest of the variables' numbers, training_frame: Training data
# hidden: size of hidden layers
mdl <- h2o.deeplearning(x=1:13, y=14, training_frame=trH, # train a DLNN on the training data
                        hidden=c(100,100,100, 100), epochs=500)
mdl
preds <- as.vector(h2o.predict(mdl,tsH)) # predict "medv" values 
preds

mean(abs(preds-as.vector(tsH$medv))) #the error rate 