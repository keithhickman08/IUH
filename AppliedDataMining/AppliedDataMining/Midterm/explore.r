## Exploratory Analysis

install.packages("data.table")
library(data.table)

mydata <- read_csv("C:/Users/khickman/Desktop/Personal/IUMSDS/AppliedDataMining/Midterm/mydata.csv")
filter(mydata, !complete.cases(mydata))
head(mydata)
describe(mydata)

mydata.clean <- mydata[!is.na(d)]
summary(mydata)

library(ggplot2)
data(mydata.clean)
sd(mydata.clean$V4, na.rm=TRUE)
var(mydata.clean$V4, na.rm=TRUE)
IQR(mydata.clean$V4, na.rm=TRUE)

barplot(mydata.clean$v5)

plot(mydata.clean$V1,mydata.clean$V2)
plot(mydata.clean$V1,mydata.clean$V3)