mydata <- mydata[,-35]
mydata <- mydata[,-2]
head(mydata)
mydata.pca <- prcomp(mydata, scale=TRUE)
z <- mydata.pca$x[,1:2]
pairs(z)
