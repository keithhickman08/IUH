anova(lm.mpg)
carsclean <- carsknn[,c(1:6,8)]
final.lm <- lm(mpg ~ .,data=carsclean)
summary(final.lm)
