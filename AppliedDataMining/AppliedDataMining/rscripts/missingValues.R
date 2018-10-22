# Online Applied Data Mining, IUB, 2017
# Case Study: Prediction Algae Blooms
# Chapter 4 : Handling Missing Values
# Data Mining with R by Luis Torgo
# Modified by Hasan Kurban
#######################################################################################
require(dplyr) # install the dplyr package
?dplyr
#######################################################################################
# 1. Removing the observations with Unknown Values
#######################################################################################
data(algae, package="DMwR2") # Loading data
dim(algae) # dimensions of the data
filter(algae, !complete.cases(algae) ) # rows with NAs
algae <- na.omit(algae) #remove rows with NAs
dim(algae)  #dimensions of the data

data(algae, package="DMwR2") #load the data again
# half of the variables values for data points 62&199 aare missing. 
algae <- algae[-c(62, 199), ] # only remove those points


require(DMwR2) # book package, use it for manyNAs package
apply(algae, 1, function(x) sum(is.na(x))) # returns # of missing values in each row
data(algae, package="DMwR2") # load the data again
manyNAs(algae, 0.2) #the rows that have a certain number of unknowns
# Default is 0.2. Remove the rows whose 20% of variables are missing
algae <- algae[-manyNAs(algae), ]  
#######################################################################################
# 2. Filling in the Unknowns with the Most Frequent Values
#######################################################################################
# mxPH variable has NA for 48th data point and we know mxPH is a nearly Gaussion.
# Thus, replace its value with the mean of mxPH 
algae[48, "mxPH"] <- mean(algae$mxPH, na.rm = TRUE)
#Chla variable has 12 NAs and its distribution is skewed to lower values.
# So, use median to replace the NAs
algae[is.na(algae$Chla), "Chla"] <- median(algae$Chla, na.rm = TRUE)
# centralImputation book package fills in all unknowns in a data set using a statis centrality
# Default for categorical variables: the mode, for numeric variables: median
data(algae, package="DMwR2") # load the data again
algae <- algae[-manyNAs(algae), ]
algae <- centralImputation(algae) 

#######################################################################################
# 3. Filling in the Unknown Values by Exploring Correlations
#######################################################################################
# A.Severel approaches to observe correlation among numerical variables
# and replace  NAs using linear models
# use="complete.obs" ignores NAs
cor(algae[, 4:18], use = "complete.obs")  # First approach
symnum(cor(algae[,4:18],use="complete.obs")) # Second approach
# Third approach
library(corrplot) 
cm <- cor(algae[,4:18], use="complete.obs")
corrplot(cm, type="upper", tl.pos="d")
corrplot(cm, add=TRUE, type="lower", method="number", 
         diag=FALSE, tl.pos="n", cl.pos="n")
#Corrlation matrix shows that  PO4 and oPO4 variables are linearly correlated.
# We will fit a linear model to these variables to replace missing value of
# PO4  variable for 28th data point
data(algae, package="DMwR2")
algae <- algae[-manyNAs(algae), ] # remove rows with NAs where 20% variables are missing
lm(PO4 ~ oPO4, data = algae) # fitting a linear model to these variables
algae[28, "PO4"] <- 42.897 + 1.293 * algae[28, "oPO4"] #replace the missing value using the model

# create a function that would return the value of PO4 given the value of oPO4
# then apply this function to all unknown values
data(algae, package="DMwR2") # load the data
algae <- algae[-manyNAs(algae), ] # remove rows with NAs where 20% variables are missing
fillPO4 <- function(oP) ifelse(is.na(oP),NA,42.897 + 1.293 * oP)
algae[is.na(algae$PO4), "PO4"] <- sapply(algae[is.na(algae$PO4), "oPO4"], fillPO4)

# B. Observe correlation among categorical and numeric variables
#load the package to fix the ordering issue of categorical variables. (forcats package)
# R alphabetically orders the categorical variables
# Rebuild the order or categorical variables
require(forcats)
require(dplyr)
require(ggplot2) # visualization package
algae <- mutate(algae,
                size=fct_relevel(size,c("small","medium","large")),
                speed=fct_relevel(speed,c("low","medium","high")),
                season=fct_relevel(season,c("spring","summer","autumn","winter")))
# this plot shows that values of mxPH are not seriously influenced by the seaons
ggplot(algae, aes(x=mxPH)) + geom_histogram(binwidth=0.5) + facet_wrap(~ season)
#mxPH, size and speed
ggplot(algae, aes(x=mxPH)) + geom_histogram(binwidth=0.5) + facet_wrap(size ~ speed)
ggplot(algae, aes(x=mxPH, y=size, color=size)) + geom_point() + facet_wrap(~speed) + geom_jitter(height = 0.4)
#######################################################################################
# 4. Filling in the Unknown Values by Exploring Similarities between Cases
data(algae, package="DMwR2") # load the data
algae <- algae[-manyNAs(algae), ] #remove rows with NAs where 20% variables are missing
?knnImputation
# Fill in NAs using KNN, k: the number of nearest neighbours 
algae <- knnImputation(algae, k = 10)
algae <- knnImputation(algae, k = 10, meth = "median")