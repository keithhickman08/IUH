library(DMwR)
library(car)

par(mfrow=c(1,2))
hist(algae$mxPH, prob=T,xlab='',
  main="Histogram of max ph value", ylim =0:1)
lines(density(algae$mxPH, na.rm=T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH, main="Normal QQ plot of maximum PH")
par(mfrow=c(1,1))


