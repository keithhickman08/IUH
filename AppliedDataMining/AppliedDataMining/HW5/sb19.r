require(ggplot2)

carframe <- data.frame(lm.mpg=lmpredict,
                       rt.mpg=treepredict,
                       true.mpg=carsclean[["mpg"]])

ggplot(carframe,aes(x=lm.mpg,y=true.mpg)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Linear Model")

ggplot(carframe,aes(x=rt.mpg,y=true.mpg)) +
  geom_point() + geom_abline(slope=1,intercept=0,color="red") +
  ggtitle("Regression Tree")
