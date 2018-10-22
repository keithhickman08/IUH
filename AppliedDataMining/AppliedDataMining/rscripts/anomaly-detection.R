#Anomoly Detection:
#Data Minnig with R Learning with case studies by Luis Targo
#Modified by Hasan Kurban - I590 Online Applied Data Mining
#Fall 2017

################################################################################

#1. Univariate Outlier Detection 
# Grubb's test: We will find the outliers in the a2 variable of algae data set
# using Grubb's test
#install the packages 
install.packages("outliers")
library(outliers)
#this function iteratively calls the  outliers package function
grubbs.outliers <- function(x, p.thresh=0.05) {
  require(outliers, quietly=TRUE)
  x <- x[!is.na(x)]
  n <- length(x)
  zs <- abs(x - mean(x)) / sd(x)
  outs <- 1 - sapply(zs, function(z) pgrubbs(z, n, type=10))
  posOuts <- which(outs <= p.thresh)
  return(list(zs=zs, 
              pvals=outs, 
              outliers=x[posOuts],
              positions=posOuts))
}
data(algae, package="DMwR2") #load data 
View(algae$a2) #Observe a2 variable
#Running Grubb's test over a2 variable
grubbs.outliers(algae$a2)$outliers
#According to Grubb's test, there are two outliers in a2 variable

################################################################################

#2. Multi-variate Outlier Detection 

#A. Finding anomalies in the Glass data set using dbscan clustering algorithm
#install packages
?dplyr # to get more information about functions
?forcats
install.packages("dplyr") #install packages
library(dplyr)
install.packages("forcats")
library(forcats)
data(Glass, package="mlbench") #load Glass data set
View(Glass) #View data
dim(Glass) # size of data
names(Glass) # print out the names of the variables
Glass$Type #There are 7 type of glasses
#Count of each glass type in the data
count(Glass,Type)  # a dplyr  alternative to "table(Glass$Type)"
g <- mutate(Glass, #This function modifies the Type varibale as  "normal" and "rare"
            Type=fct_collapse(Type,
                              rare   = as.character(c(3,5,6)),
                              normal = as.character(c(1,2,7))
            )
)
g %>% count(Type) %>% mutate(prop=100*n/nrow(g))
#There are two types of glasses in the data now and "g" is the new data

# "dbscan.outliers" function uses dbscan, density based clustering algorithm,
# to detect the outliers in the data.
# Load the dbscan package and the  dbscan.outliers function
?dbscan # learn more about dbscan package
install.packages("dbscan")
library(dbscan)
dbscan.outliers <- function(data, ...) {
  require(fpc, quietly=TRUE)
  cl <- dbscan(data, ...)
  posOuts <- which(cl$cluster == 0)
  list(positions = posOuts,
       outliers = data[posOuts,], 
       dbscanResults = cl)
}


g[,-10] <- scale(g[,-10]) # scaling glass data
outs <- dbscan.outliers(g[,-10], eps=1) # call dbscan.outliers to find the outliers
head(outs$outliers) #print out the first 6 outliers
nrow(outs$outliers) #number of outliers
slice(g, outs$positions) %>%  count(Type) #Count of outliers for each glass Type
count(g, Type) # count of each glass type



#B: Detection of anamolies in the Glass data set using OR_h method
# OR_h uses hierarchical agglomerative clustering 
library(DMwR2) #install the packages
library(dplyr)
og <- outliers.ranking(select(g, -Type)) # running OR_h over Glass data set
# Return top 40 outliers - count of outliers for each glass Type
slice(g, og$rank.outliers[1:40]) %>%  count(Type) 


#C: Detection of anamolies in the Glass data set using LOF method
#Outliers are the data points located in the region with low density.
# make use of KNN (k-nearest neighbor algorithm)
library(DMwR2)
library(dplyr)
lof.scores <- lofactor(select(g, -Type),10)  #running LOF over Glass data set
# Return top 40 outliers - count of outliers for each glass Type
slice(g, order(lof.scores,decreasing=TRUE)[1:40]) %>%  count(Type)



#D. We have showed examples of unsupervised outlier techniques so far.
# We here detect the outliers using SVM algorithm - supervised outlier detection approach

install.packages("e1071") # package for svm algorithm
library(e1071)
?svm  # to get familiar with svm
# Train the one-class SVM on the normal cases only in the glass data set
trainD <- filter(g, Type == "normal") %>% select(-Type) 
s <- svm(trainD, y=NULL, type="one-classification", nu=0.5)
#Check the ones that are not predicted as normal -- predicted value: FALSE
# Confusing matrix for prediction
(cm <- table(g$Type, predict(s,select(g, -Type))))

#In summary, we observe different approaches for outlier detection result in different results 