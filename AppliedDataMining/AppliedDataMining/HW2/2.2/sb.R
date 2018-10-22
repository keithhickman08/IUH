install.packages("Hmisc")
library(Hmisc)

mydata <- fread("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data")
describe(mydata)
