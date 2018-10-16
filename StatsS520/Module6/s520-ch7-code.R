### R examples relevant to ch. 7

################### Part 1: Reading data into R  #######################

# Enter data manually using c():

die = c(1, 2, 3, 4, 5, 6)

# Enter data as one variable:

nerve = scan("nerve.txt")
nerve = scan(file.choose())

# Enter data as a "data frame" (table):

singer = read.table("singer.txt", header=TRUE)
# To get the variable "height" from the data frame:
singer$height
# To get the first ten numbers in singer$height:
singer$height[1:10]

# Scanning data from the web: Earthquake magnitudes

earthquake.data = read.table("http://service.scedc.caltech.edu/ftp/catalogs/SCEC_DC/2014.catalog")
head(earthquake.data)
# Magnitude is the fifth column
magnitude = earthquake.data[,5]
length(magnitude)
nrow(earthquake.data)

################### Part 2: The plug-in principle  #######################

# Estimate based on data
# n.b. not a random sample but should be independent
# according to seismologists
# ECDF at y=1
sum(magnitude <= 1) / length(magnitude)

# CDF
plot(ecdf(magnitude)) # Chapter 7 lecture note
plot.ecdf(magnitude) # textbook pg 156
# dev.print(pdf, "myGreatGraph.pdf")

# Numerical summaries
mean(magnitude)
mean(magnitude^2) - mean(magnitude)^2 # plug-in variance
sqrt(mean(magnitude^2) - mean(magnitude)^2) # plug-in standard deviation
var(magnitude) # sample variance
sd(magnitude) # sample standard deviation
median(magnitude) 
quantile(magnitude, 0.25)
quantile(magnitude, 0.75)

################### Part 3: The five-number summary and the boxplot  #######################

summary(magnitude)
boxplot(magnitude)
boxplot(magnitude, range=0)

# Comparison

earthquake2013 = read.table("http://service.scedc.caltech.edu/ftp/catalogs/SCEC_DC/2013.catalog")
magnitude2013 = earthquake2013[,5]
boxplot(magnitude2013, magnitude, range=0,
  main="Boxplots of SCEC earthquake catalog magnitudes",
  ylab="Magnitude",
  names=c(2013, 2014))

################### Part 4: QQ plot or Normal probability plot  #######################

### What is a QQ plot?

x = rnorm(50, 0, 1)
sort(x)

# What should the values be?
qnorm(seq(0.01, 0.99, 0.02))
plot(qnorm(seq(0.01, 0.99, 0.02)),
     sort(x))

x1 = rnorm(50, mean=10, sd=20)
# Is this a straight line
plot(qnorm(seq(0.01, 0.99, 0.02)),
     sort(x1))
# Much easier way
qqnorm(x1)

x2 = runif(50)
qqnorm(x2)

x3 = rexp(50)
qqnorm(x3)

x4 = rnorm(20)
qqnorm(x4)


### Heights of the New York Choral Society
### singer.txt

singer = read.table("singer.txt", header=TRUE)
head(singer)
singer$height
length(singer$height)
summary(singer$height)
var(singer$height)
sd(singer$height)
plot(ecdf(singer$height))
boxplot(singer$height)
qqnorm(singer$height)
fakeheights = singer$height +
  runif(235, min=-0.5, max=0.5)
qqnorm(fakeheights)


################### Part 5: Kernel Density Plot  #######################


hist(singer$height,
     breaks=59.5:76.5)
plot(density(singer$height))

# Wait, what's a density plot?

plot(density(singer$height[1:2]))
plot(density(singer$height[1:3]))
plot(density(singer$height[1:4]))
plot(density(singer$height[1:5]))
plot(density(singer$height[1:10]))
plot(density(singer$height[1:30]))
plot(density(singer$height[1:100]))
plot(density(singer$height))

# Look at men and women separately

womens.heights = singer$height[1:128]
mens.heights = singer$height[129:235]
summary(womens.heights)
summary(mens.heights)
boxplot(womens.heights, mens.heights)

hist(mens.heights, breaks=63.5:76.5)
qqnorm(mens.heights + 
  runif(107, min=-0.5, max=0.5))

boxplot(singer$height~singer$voice.part,
  cex.axis=0.7, #makes x labels smaller
  ylab="Height (in)")

################### Part 6: Another Example #######################


### Stereogram fusion times
### stereograms.txt
### See https://www.brainbashers.com/stereo.asp

stereograms = read.table("stereograms.txt",
                         header=TRUE)
head(stereograms)
summary(stereograms)
boxplot(stereograms$time)
boxplot(stereograms$time~stereograms$group)
hist(stereograms$time[stereograms$group==1])
hist(stereograms$time[stereograms$group==2])
qqnorm(stereograms$time[stereograms$group==1])
qqnorm(stereograms$time[stereograms$group==2])
qqnorm(log(stereograms$time[stereograms$group==1]))
qqnorm(log(stereograms$time[stereograms$group==2]))



# PDF
hist(magnitude)
hist(magnitude, prob=TRUE)
plot(density(magnitude)) # Smooth version of histogram
# Compare to exponential
par(mfrow=c(2,1))
curve(dexp, from=0, to=5)
plot(density(magnitude), xlim=c(2.5,5), ylim=c(0,0.1))
par(mfrow=c(1,1))



