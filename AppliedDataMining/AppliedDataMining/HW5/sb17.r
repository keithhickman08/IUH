treepredict <- predict(final.tree, carsclean)
lmpredict <- predict(final.lm, carsclean)
summary(treepredict)
summary(lmpredict)

mse.treepredict <- mean(treepredict - carsclean[["mpg"]])^2
mse.lmpredict <- mean(lmpredict - carsclean[["mpg"]])^2
