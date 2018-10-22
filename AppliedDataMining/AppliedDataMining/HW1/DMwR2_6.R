data(algae)
algae <- algae[-manyNAs(algae)]
clean.algae <- knnImputation(algae, k=10)
summary(clean.algae)

## create linear regression model for a1 variable with all other variables. 
lm.ai <- lm(a1 ~ ., data = clean.algae[, 1:12])
lm.ai