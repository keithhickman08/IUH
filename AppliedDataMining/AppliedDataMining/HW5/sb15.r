require(rpart)
require(rpart.plot)

final.tree <- rpart(mpg ~ .,data=carsclean)
summary(final.tree)
plot(final.tree)
