library(DMwR)
library(car)

par(mfrow=c(1,2))
hist(algae$mxPH, prob=T,xlab='',
     main="Histogram of max ph value", ylim =0:1)
lines(density(algae$mxPH, na.rm=T))
rug(jitter(algae$mxPH))
qq.plot(algae$mxPH, main="Normal QQ plot of maximum PH")
par(mfrow=c(1,1))
summary(algae)

plot(algae$NH4)
abline(h=mean(algae$NH4,na.rm=T))
abline(h=mean(algae$NH4, na.rm=T)+sd(algae$NH4,na.rm=T))
abline(h=median(algae$NH4, na.rm=T))
identify(algae$NH4)
## this instruction allows for interaction

boxplot(algae$oPO4, ylab = "Orthophosphate OPO4")
rug(jitter(algae$oPO4), side = 2)
abline(h=mean(algae$oPO4, na.rm=T), lty=2)