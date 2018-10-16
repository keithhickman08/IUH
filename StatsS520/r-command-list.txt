### MASTER LIST OF R COMMANDS ###

An asterisk (*) indicates a less important command for this course.



# Getting help

?: Help. e.g. If you want the helpfile on dbinom, type ?dbinom



# Probability distributions: The d/p/q/r format

dbinom(x,n,p): Calculates f(x), the probability that a binomial random variable with n trials and success probability p is exactly equal to x.

pbinom(x,n,p): Calculates F(x), the probability that a binomial random variable with n trials and success probability p is less than or equal to x.

*qbinom(q,n,p): Calculates the q-quantile of a binomial random variable with n trials and success probability p.

*rbinom(N,n,p): Generates N realizations of a binomial random variable with n trials and success probability p.

*dnorm(x, mean, sd): Calculates f(x), the PDF of a normal random variable. If no mean and SD are specified, the mean is 0 and the SD is 1.

pnorm(x, mean, sd): Calculates F(x), the CDF of a normal random variable. If no mean and SD are specified, the mean is 0 and the SD is 1.

qnorm(q, mean, sd): Calculates the q-quantile of a normal random variable. If no mean and SD are specified, the mean is 0 and the SD is 1.

rnorm(N, mean, sd): Generates N realizations of a normal random variable. If no mean and SD are specified, the mean is 0 and the SD is 1.

Note: The d/p/q/r format holds for many probability distributions, e.g. the functions dexp(), pexp(), qexp(), and rexp() exist for the exponential distribution.

pt(x, df=nu): Calculates the probability that a t random variable with nu degrees of freedom is less than or equal to x.

qt(q, df=nu): Calculates the q-quantile of a t random variable with nu degrees of freedom. Used in t confidence intervals.

pf(x, df1, df2): Calculates the probability that a F random variable with df1 and df2 degrees of freedom is less than or equal to x.

pchisq(x, df): Calculates the probability that a chi-square random variable with df degrees of freedom is less than or equal to x.

*rlnorm(n): Generates N realizations of a standard lognormal random variable.

*dpois(x,lambda): Calculates f(x), the probability that a Poisson random variable with parameter lambda is exactly equal to x.

*ppois(x,lambda): Calculates F(x), the probability that a Poisson random variable with parameter lambda is less than or equal to x.



# Entering data into R

c(): Creates a vector, e.g. x=c(1,2,3) creates the vector (1,2,3). Can also be used to combine existing vectors, e.g. c(x,y) combines the vectors x and y.

scan(): Reads a list of data in a file into R as a vector.

read.table("filename"): Function for reading data in tabular form into R. By default, R assumes values are separated by whitespace, with no header. If they're separated by commas, use
read.table("filename", sep=",")
If they're separated by whitespace but there's a header, use
read.table("filename", header=T)
The filename may be a URL. If you have saved the file to disk but don't remember the path, use
read.table(file.choose())

file.choose(): Brings up a GUI to allow you to choose a file.

*data.frame: One way of storing multivariate data in R. e.g. To create a data frame that contains variables x and y:
somedata = data.frame(x,y)
Then
somedata$x
returns the x-variable.

*attach(data): Lets you call the variables in the data frame "data" by their individual names. e.g. If data contains x and y, you can now ask R for
x + y
Not recommended because it’s easy to forget what data is attached and what isn’t.

*detach(data): Undoes the attach command.

*seq(): Creates a vector containing a sequence of equally-spaced numbers, e.g.
x = seq(-4, 4, 0.01)
creates a sequence of numbers from -4 to 4 at a spacing of 0.01.



# Math

sum(x): Calculates the sum of data x.

sqrt(x): Calculates the square root of x.

log(x): Calculates the log (base e) of x.

exp(x): Calculates the exponential of x, i.e. 2.718^x.



# Data summary

head(x): Displays the first few elements of a data set x.

length(x): Returns the length of x.

median(x): Calculates the median of data x.

quantile(x, 0.8): Calculates the 0.8-empirical quantile of data x.

mean(x): Calculates the mean of data x.

summary(x): Gives a numerical summary of x. If x is a numerical vector, the command gives the minimum, first quartile, median, mean, upper quartile, and maximum. If x is a model, the command may give estimated coefficients and their errors, test statistics, and P-values.

sd(x): Calculates the sample standard devation (s) of x, using the n-1 denominator.

var(x): Calculates the sample variance of x, using the n-1 denominator.

cor(x, y): Calculates the (Pearson's product-moment) correlation coefficient of x and y.



# Graphics

plot.ecdf(x): Graphs the empirical distribution function of data x.

boxplot(x): Draws a boxplot of data x. To plot without outlier identification, do
boxplot(x, range=0).

hist(x): Draws a histogram of counts in data x. You can specify the bars, e.g. if all the data are between 0 and 10, then
hist(x, breaks=0:10)
gives you bars of width 1.

hist(x, prob=T): Draws a probability histogram of data x (i.e. the total area is 1).

plot(): General purpose plot command. There are lots of arguments you can use inside the parentheses to make your plots look nice, e.g. for a labeled scatter plot:
plot(x, x^2, xlab="This is the x label", ylab="This is the y label", main="This is the title")

plot(density(x)): Graphs a density plot of data x. There are several options for changing the amount of smoothing, e.g.
plot(density(x), adjust=2))

curve(): Plots a specified function or curve, e.g.
curve(dnorm(x, mean=10, sd=2), from=0, to=20)
plots the pdf of a normal with mean 10 and sd 2, for x-values from 0 to 20.

qqnorm(x): Draws a "normal probability plot" that plot quantiles of the sample against quantiles of a standard normal distribution. If the sample comes from a normal distribution, this plot will give an approximate straight line (except maybe at the corners).

*abline(): Adds a line to an existing plot. e.g.
abline(v=5, col="red")
adds a red vertical (v) line to the plot at x=5.
abline(3, 4, col="blue")
adds a blue line with x-intercept 3 and slope 4 to the plot.

*lines(x, y): Draws a line on top of an existing graph.

*par(): Sets graphics parameters. One useful option is
par(mfrow=c(2,2))
which sets up a 2x2 matrix of graphs. Then you can fit four graphs on one page. If you want to return to one graph per page, do
par(mfrow=c(1,1))



# Miscellaneous useful stuff

rep(x,n): Creates a vector that consists of x repeated n times.

sample(): Draws a random sample. e.g.
sample(x, size=n, replace=TRUE)
draws a random sample of size n from population x, with replacement.

function(): Creates an R function.
Example: A function called "roulette":
roulette = function(n){
	prizes = c(35, rep(-1,37))
	x = sample(prizes, size=n, replace=TRUE)
	return(sum(x))
}
Then roulette(n) simulates the total profit in n games of roulette.

return(): Specifies the output of a function (see above).

*replicate(): Replicates a command, e.g.
replicate(10, roulette(365))
replicates the roulette function with n=365 ten times.

ls(): Lists objects currently in R's memory.



# Dark magic

*t.test(x,y): Performs a Welch two-sample t-test on x and y.

*lm(y ~ x): Fits a linear regression, using x to predict y.

*predict(model, newdata, interval="prediction"): Gives you a 95% prediction interval for predictors in newdata using "model". Quite a tricky command to use: newdata must be carefully specified. Ask for help if you need it.

*anova(model): Finds the analysis of variance table for a model. Easy to use for regression models. Using it for group comparisons is beyond this course.
