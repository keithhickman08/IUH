summary(algae$mxPH)
is.na(algae$mxPH)
## finds na values, probably an easier way
algae[48,]
## calls observation with NA.
## find corrlated variable with: 
cor(algae[, 4:18], use = "complete.obs")
lm(mxPH ~ Chla, data = algae)
## get values from formula
algae[48, "mxPH"] <- 7.92896 + .01047 * algae[48, "Chla"]
## formula = missing mxPH variable + intercept + beta coeff * present var