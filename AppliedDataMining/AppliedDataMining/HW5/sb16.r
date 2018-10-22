treepredict <- predict(final.tree, carsclean)
lmpredict <- predict(final.lm, carsclean)
summary(treepredict)
summary(lmpredict)

mae.treepredict <- mean(abs(treepredict - carsclean[["mpg"]]))
mae.lmpredict <- mean(abs(lmpredict - carsclean[["mpg"]]))
