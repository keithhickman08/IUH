# PCA with prcomp() 
# to get more information about prcomp
?prcomp
dim(iris)
data(iris)
# remove class variable
pca.data <- iris[,-5] 
#PCA with prcomp() 
pca <- prcomp(pca.data)
# importance of variables - variance captured by each principal component
#old variables: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
#new variables (principal components): PC1, PC2, PC3, PC4
summary(pca) 
names(iris)
#How many principal components: scree plot
plot(pca, type = "l")
screeplot(pca)
#newdata
View(pca$x)
dim(pca$x)
pca$x[1:10,] #print out the first 10 data points of the transformed data.
#Print out the first two principal components
pca$x[,1:2]
#scatter plot of the first two principal components
pairs(pca$x[,1:2])
# principal components are linear combinations of the original variables 
#For example: PC1: 0.36138659 * Sepal.Length + -0.08452251 *Sepal.Width 
#                  + 0.85667061 * Petal.Length + 0.35828920 * Petal.Width 
pca$rotation  #rotation or loadings
#reduced data has captured 98% of the variance in Iris data. 
# We represent Iris data as 2 dimensional data
reduced.iris <- data.frame(pca$x[,1:2],Species=iris$Species)
#observe data, old variables and new variables(principal components)
biplot(pca,scale=0)
########################################################################################
#PCA with princomp()
#princomp()
?princomp
data(iris)
pca.data <- iris[,-5] 
#PCA with princomp() 
pca <- princomp(pca.data)
# principal components are linear combinations of the original variables 
loadings(pca)
View(pca$scores) #data after transformation
dim(iris)
#adding class variable to new data set
reduced.iris <- data.frame(pca$scores[,1:2],Species=iris$Species)
dim(reduced.iris)
head(reduced.iris)