# Online Applied Data Mining, Fall 2017
# K-means clustering algorithm
# An introduction to Statistical Learning 
# Modified by Hasan Kurban
###############################################################################
#generate data: 2-dimensional 100 data points from standard normal distribution
mydata=matrix(rnorm(100*2), ncol=2)
mydata[1:50,1]=mydata[1:50,1]+3
mydata[1:50,2]=mydata[1:50,2]-4
plot(mydata[,1],mydata[,2])
View(mydata) #View data
###############################################################################
?kmeans  #to get more information about kmeans function
#Cluster mydata with k-means, k=2
#nstart: how many random sets should be chosen?
#nstart = 20 and k = 2 means that 20 times, randomly choose 2 initial points
#and initialize k-means algorithm with these initial points 
#and return the result for the best initial points (SSE lowest)
km.out=kmeans(mydata,centers=2,nstart=20)
km.out
km.out$cluster #print out the clusters
#plot the clusters
plot(mydata, col=(km.out$cluster+1), main="K-Means Clustering Results with K=2",
     xlab="", ylab="", pch=20, cex=2)
# Total within-cluster sum of squares
km.out$tot.withinss
# within-cluster sum of squares for each cluster
km.out$withinss
#k=3: Run k-means for 3-clusters
km.out=kmeans(mydata,3,nstart=20)
km.out
plot(mydata, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", 
     ylab="", pch=20, cex=2)
#Total within-cluster sum of squares
km.out$tot.withinss
# within-cluster sum of squares for each cluster
km.out$withinss

###########################################################################################
#Finding optimal k - elbow technique
k_max <- 10 
#Total within-cluster sum of squares
 tsse <- sapply(1:k_max, 
               function(k){kmeans(mydata, k, nstart=30,iter.max = 12 )$tot.withinss})
plot(1:k_max, tsse,
            type="b", pch = 20, frame = FALSE, 
            xlab="Number of clusters k",
            ylab="Total within-clusters sum of squares")