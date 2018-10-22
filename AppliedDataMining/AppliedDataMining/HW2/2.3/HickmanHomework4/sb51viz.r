#Homework 4
# Problem 5 - Visualization

install.packages("nnet")
library(nnet)
install.packages("ggplot2")

library(ggplot2)
install.packages("NeuralNetTools")
library(NeuralNetTools)

garson(n1) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plotnet(n1) 

garson(n2) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
plotnet(n2)