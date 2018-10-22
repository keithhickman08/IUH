head(cars)
cars$origin <- as.factor(unlist(cars$origin))
cars$cylinders <- as.factor(unlist(cars$cylinders))
cars$modelyear <- as.factor(unlist(cars$modelyear))
cars$newweight <- cut(cars$weight,5,c("very light", "light", "medium", "heavy", "very heavy"))

boxplot(mpg ~ origin, cars, main="MPG of Cars")

##hist(cars$weight, main="Weight of Cars", xlab="Weight in lbs", ylab="Count")
ggplot(cars,aes(x=origin, y=mpg)) + geom_violin()
