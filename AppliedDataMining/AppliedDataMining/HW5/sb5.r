install.packages("forcats")
library(forcats)
library(dplyr)

cars2graph <- filter(cars,!is.na(weight)) %>%
  mutate(weight2=cut(weight, quantile(weight,c(0,.25,.5,.75,1))))
ggplot(cars,aes(x=mpg, y=newweight, color=origin)) + geom_point()