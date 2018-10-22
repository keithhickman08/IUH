# Online Applied Data Mining, IUB, 2017
# Case Study: Prediction Algae Blooms
# Chapter 4 : Data summary and visualization
# Data Mining with R by Luis Torgo
# Modified by Hasan Kurban
########################################################################################
#Loading data

#load data from DMwR2 package
data(algae, package="DMwR2")

#read analysis.txt file to load the data
algae <- read.table('Analysis.txt',
                    header=FALSE,
                    dec='.',
                    col.names=c('season','size','speed','mxPH','mnO2','Cl',
                                'NO3','NH4','oPO4','PO4','Chla','a1','a2','a3','a4',
                                'a5','a6','a7'),
                    na.strings=c('XXXXXXX'))
########################################################################################
#Basic Summary
########################################################################################

summary(algae) # summary of the variables
dim(algae) #size of data
names(algae)
View(algae) 
########################################################################################
# Understanding mxHP variable (continuous)-- Histogram, Kernel density estimation, Q-Q plot
########################################################################################
#histogram of variable "mxHP" with probabilities for each interval of value
#instead of frequency counts
# observe that "mxPH" looks like a Gaussian distribution
require(ggplot2) # load visualization package
ggplot(algae,aes(x=mxPH)) + geom_histogram(aes(y=..density..))


require(car) # loading car package to use qqplot function
#qqplot: to check normality of data
#plot the variable values against the theoretical quantiles of a
#normal distribution
# red dashed lines includes the data with 95%  confidenence interval of the
#normal distribution assumption
#observe that some data points breaks the assumption
qqPlot(algae$mxPH,main='Normal QQ plot of maximum pH',ylab="")

#Observe the kernel density estimation over the histogram of variable "mxHP"
ggplot(algae,aes(x=mxPH)) + 
  geom_histogram(aes(y=..density..)) + 
  geom_density(color="red") + geom_rug() + 
  ggtitle("The Histogram of mxPH (maximum pH)") + 
  xlab("") + ylab("")
########################################################################################
# Understandin oPO4 variable (continuous) -- Box plot -- 5 number summary
# Min, 1st,3rd quartiles, median, Max -- the dots are outliers
########################################################################################

#read line is the mean 
# geom_rug(): showing the values of the variable
# Result for oPO4 variable: skewed distribution
ggplot(algae,aes(x=factor(0),y=oPO4)) + 
  geom_boxplot() + geom_rug() + 
  geom_hline(aes(yintercept=mean(algae$oPO4, na.rm = TRUE)),
             linetype=2,colour="red") +
  ylab("Orthophosphate (oPO4)") + xlab("") + scale_x_discrete(breaks=NULL)

########################################################################################
# Understanding "NH4" variable (continuous) -- Plot
########################################################################################

plot(algae$NH4, xlab = "") # plot values of NH4
abline(h = mean(algae$NH4, na.rm = T), lty = 1, col ="red") # redline for mean
#blue line: mean + one standard deviation
abline(h = mean(algae$NH4, na.rm = T) + sd(algae$NH4, na.rm = T), lty = 2, col = "blue")
#black line: median
abline(h = median(algae$NH4, na.rm = T), lty = 3, col = "black")
#Click on the the dots with left mouse buttion to get the row number
#Click on ESC to exit
identify(algae$NH4)
?identify

#observing the clicked data points in a data frame
plot(algae$NH4, xlab = "")
clickedRows <- identify(algae$NH4)
algae[clickedRows, ]

#observing specific data points without plotting them with "filter function"
require(dplyr) # load the package
filter(algae, NH4 > 19000)


########################################################################################
# Conditional Plot 1: How "size" variable (nominal) affect the distribution of "a1" variable (continuous)
# using box plot
########################################################################################
#load the package to fix the ordering issue of categorical variables.
require(forcats)
# R alphabetically orders the categorical variables
# Rebuild the order or categorical variables
algae <- mutate(algae,
                size=fct_relevel(size,c("small","medium","large")),
                speed=fct_relevel(speed,c("low","medium","high")),
                season=fct_relevel(season,c("spring","summer","autumn","winter")))


# observe how  the distribution of "a1" variable looks with the "size" variable.
ggplot(algae,aes(x=size,y=a1)) + geom_boxplot() +
  xlab("River Size") + ylab("Algal A1")
# we observe that higher frequencies of algae "a1" are expected in smaller rivers

# same idea with violin plots: give more idea about distribution of data
# geom_jitter() shows the data points in the plot -- similar to geom_point()
ggplot(algae,aes(x=size,y=a1)) + 
  geom_violin() + geom_jitter() + xlab("River Size") + ylab("Algal A1")

########################################################################################
#Conditional Plot 2: "a3" variable (continuous) conditioned by season (categorical)
# and mnO2 (continuous)
########################################################################################
data2graph <- filter(algae,!is.na(mnO2)) %>% #remove rows with NAs
#creating a new colum column "minO2" that is discretized version of "mnO2"  using "cut" function
mutate(minO2=cut(mnO2, quantile(mnO2,c(0,0.25,.5,.75,1)), include.lowest=TRUE))
ggplot(data2graph,aes(x=a3,y=season, color=season)) + geom_point() + 
  facet_wrap(~ minO2) + 
  guides(color=FALSE)