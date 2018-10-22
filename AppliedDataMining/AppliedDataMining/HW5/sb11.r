??lm
carsknn
carslm <- lm(mpg ~ weight, carsknn)
summary(carslm)

plot(carslm)

plot(x=carsknn$mpg, y=carsknn$weight)
