# Comments in R are written using hash(#)
# The recommended style for commenting in R is having commented lines begin with # and one space.

# Basic Calculations
2+2
2*3
8/2
2^3
pi
sin(pi)
cos(pi)
tan(pi)

# Functions
sqrt(4)
abs(-65)
?sqrt

# Variables
SquareRoot4 = sqrt(4)
SquareRoot4
HoursWeek <- 7*24   #Both <- and = can be used to assign variables
HoursWeek
ls()

# Setting working directory
# the path format may be change on Mac.
setwd("C:/Users/Krsna/Programming/R")
getwd()

# Installing packages
install.packages("ggplot2")

# Loading package
library("ggplot2")

# To plot
ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species)) + geom_point(size=3)


