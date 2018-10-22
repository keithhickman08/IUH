library(DMwR)

carsknn <- knnImputation(cars, k=5, meth="median")
summary(carsknn)
