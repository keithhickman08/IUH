lm.mpg <- lm(mpg ~ ., data=carsknn)
## lm.mpg.weight <- lm(mpg ~ weight, data = carsknn)
summary(lm.mpg)
## summary(lm.mpg.weight)
plot(lm.mpg)

anova(lm.mpg)
