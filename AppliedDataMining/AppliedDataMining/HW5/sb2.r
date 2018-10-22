library(data.table)
library(car)
library(ggplot2)

??fread
cars <- fread("cars.csv", header=TRUE)
cars <- cars[,c(0:7,9)]
head(cars)
summary(cars)

##hist(cars$acceleration)
##hist(cars$horsepower)
##hist(cars$weight)
##hist(cars$mpg)
##hist(cars$cylinders)

hist(cars$displacement, main="Displacement", xlab="Categories of Displacement in CC")

qqPlot(cars$displacement, main="QQ Plot of Displacement", ylab="Displacement in cc")

boxplot(cars$weight)
