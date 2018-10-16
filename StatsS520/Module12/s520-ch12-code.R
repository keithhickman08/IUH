### Example: Equal sample sizes

# Set random seed

set.seed(123456)

# Generate random data

x1 = rnorm(50, mean=50, sd=15)
x2 = rnorm(50, mean=50, sd=15)
x3 = rnorm(50, mean=50, sd=15)

boxplot(x1, x2, x3)

y1 = rnorm(50, mean=50, sd=15)
y2 = rnorm(50, mean=50, sd=15)
y3 = rnorm(50, mean=60, sd=15)

boxplot(y1, y2, y3)

sd(x1)
sd(x2)
sd(x3)

# "Within-sample" estimate of variance

(var(x1) + var(x2) + var(x3)) / 3

# "Between-sample" estimate of variance

var(c(mean(x1), mean(x2), mean(x3))) * 50

# Same things for y
# Within-sample
(var(y1) + var(y2) + var(y3)) / 3
# Between-sample
var(c(mean(y1), mean(y2), mean(y3))) * 50

# Calculate an F-statistic
within.ms = (var(x1) + var(x2) + var(x3)) / 3
between.ms = var(c(mean(x1), mean(x2), mean(x3))) * 50
F = between.ms / within.ms

# P-value: Compare to F distribution
1 - pf(F, df1=2, df2=147)

# Same things for y
within.ms = (var(y1) + var(y2) + var(y3)) / 3
between.ms = var(c(mean(y1), mean(y2), mean(y3))) * 50
F = between.ms / within.ms
1 - pf(F, df1=2, df2=147)

# Shortcut

x.all = c(x1, x2, x3)
group = factor(c(rep(1, 50), rep(2, 50), rep(3, 50)))
anova(lm(x.all ~ group))

y.all = c(y1, y2, y3)
group = factor(c(rep(1, 50), rep(2, 50), rep(3, 50)))
anova(lm(y.all ~ group))





### Anorexia example

# Raw data at:
# http://mypage.iu.edu/~mtrosset/StatInfeR/Data/anorexia.dat
# We'll use the file anorexia.txt
anorexia = read.table("anorexia.txt",
                      header=TRUE)
summary(anorexia)
all.diffs = anorexia$After - anorexia$Before
cog.diff = all.diffs[anorexia$Treatment=="Cognitive"]
fam.diff = all.diffs[anorexia$Treatment=="Family"]
std.diff = all.diffs[anorexia$Treatment=="Standard"]
cog.diff
fam.diff
std.diff
n1 = length(cog.diff)
n2 = length(fam.diff)
n3 = length(std.diff)
N = n1 + n2 + n3
boxplot(cog.diff, fam.diff, std.diff)
# Better-looking boxplots
boxplot(cog.diff, fam.diff, std.diff, range=0)
boxplot(cog.diff, fam.diff, std.diff, range=0, 
    names=c("Cognitive","Family","Standard"),
    ylab="Change in weight (pounds)")

# Check homoscedasticity
sd(cog.diff)
sd(fam.diff)
sd(std.diff)

# Check normality
qqnorm(cog.diff)
qqnorm(fam.diff)
qqnorm(std.diff)

# Find group means and grand mean
cog.mean = mean(cog.diff)
fam.mean = mean(fam.diff)
std.mean = mean(std.diff)
grand.mean = mean(all.diffs)

# Total sum-of-squares
SST = sum( (all.diffs-grand.mean)^2 )
total.df = N - 1

# Between sum-of-squares and mean-square
SSB = n1*(cog.mean-grand.mean)^2 +
    n2*(fam.mean-grand.mean)^2 +
    n3*(std.mean-grand.mean)^2
between.df = 2 
between.meansquare = SSB/2

# Within sum-of-squares and mean-square
SSW = sum( (cog.diff-cog.mean)^2 ) +
    sum( (fam.diff-fam.mean)^2 ) +
    sum( (std.diff-std.mean)^2 )
# Alternative formula
SSW = (n1-1)*var(cog.diff) +
  (n2-1)*var(fam.diff) +
  (n3-1)*var(std.diff)
within.df = N - 3
within.meansquare = SSW/within.df

# Check these are equal
SST
SSB + SSW

# Are these close?
between.meansquare
within.meansquare

# F-test
F = between.meansquare/within.meansquare
# P-value
1 - pf(F, df1=between.df, df2=within.df)

# Shortcut

anova(lm(all.diffs ~ anorexia$Treatment))

### Extra stuff
# (Not examinable)
# Don't assume normality
# How do we get a P-value?

F.random = function(){
  random.diffs = sample(all.diffs)
  cog.random = random.diffs[1:29]
  fam.random = random.diffs[30:46]
  std.random = random.diffs[47:72]
  SSW.random = (n1-1)*var(cog.random) +
    (n2-1)*var(fam.random) + 
    (n3-1)*var(std.random)
  SSB.random = SST - SSW.random
 return((SSB.random/between.df) / (SSW.random/within.df))
}

F.list = replicate(100000, F.random())
mean(F.list >= F)





### So what's the difference?

# t-test cog vs. std and fam vs. std
# Note: Because we're doing two tests,
# compare P-values to 0.025
t.test(cog.diff, std.diff)
t.test(fam.diff, std.diff)

### Jellybean demo
# https://xkcd.com/882/


# Suppose we set up a randomized experiment
# Measure "normalized acne"
set.seed(520)
control = rnorm(1000)
jellybeans = rnorm(2000)
jellybean.matrix = matrix(jellybeans, nrow=100, ncol=20)
t.test(jellybeans, control, alt="greater")
t.test(jellybeans, control,
      alt="greater")$p.value

# Does color matter?
boxplot(jellybean.matrix, range=0)
# Naive way: Do 20 tests at level alpha = 0.05
P.values = rep(NA, 20)
for(J in 1:20){
  P.values[J] = t.test(jellybean.matrix[,J], control,
      alt="greater")$p.value
}
# Which tests
P.values < 0.05
#3 has a small P-value
green.beans = jellybean.matrix[,3]
t.test(green.beans, control, alt="greater")
# Green jelly beans cause acne
# is a terrible conclusion

# Better approaches:
# 1. Do an ANOVA to see if color matters
color = factor(rep(1:20, each=100))
anova(lm(jellybeans ~ color))

# 2. Instead of comparing to alpha,
# compare to alpha/number of tests
P.values < 0.05/20
# Green jelly beans do not cause acne




