## Homework Problem Set 6
## Keith Hickman

ex7 <- read.table("c:/Users/khickman/Desktop/Personal/IUMSDS/StatsS520/Module6/ex7.csv")
data <- ex7
summary(data)
data <- data[,1]
plot(ecdf(data))
