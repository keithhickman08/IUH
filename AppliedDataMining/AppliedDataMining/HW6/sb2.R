#Read in the data

train <- fread("C:\\Users\\khickman\\Desktop\\Personal\\IUMSDS\\AppliedDataMining\\HW6\\train.csv")
test <- fread("C:\\Users\\khickman\\Desktop\\Personal\\IUMSDS\\AppliedDataMining\\HW6\\test.csv")
summary(train)
str(train)