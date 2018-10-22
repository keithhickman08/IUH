mydata <- mydata[,-35]
head(mydata)
kmeans1 <- kmeans(mydata,2,nstart=50)
kmeans1$tot.withinss

k_max <- 10
#total SSE
tsse <- sapply(1:k_max, function(k){kmeans(mydata, k, nstart=30,iter.max = 12 )$tot.withinss})
tsse
plot(1:k_max, tsse, type="b", pch = 20, frame = FALSE, xlab="Number of clusters k",
     ylab="Total within-clusters sum of squares")
