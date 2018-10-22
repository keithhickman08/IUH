summary(cars)
install.packages("corrplot")
library(corrplot)
cm <- cor(cars[,c(1,3,4,5,6)],use="complete.obs")
corrplot(cm)

cor(cars$mpg, cars$horsepower)

lm(displacement ~ horsepower, data = cars)

fillhorsepower <- function(hp) ifelse(is.na(hp),NA,-60.59 + 2.44 * hp)
cars[is.na(cars$horsepower), "horsepower"] <- sapply(cars[is.na(cars$horsepower), "displacement"], fillhorsepower)
