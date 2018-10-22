summary(territorydist)

territorydist <- na.omit(territorydist)
## territorydist$`Territory Code`

boxplot(standardized ~ territorycode, territorydist, main="Invoiced Amount Distribution")

boxplot(weight ~ cylinders, cars, main="Weight of Cars")