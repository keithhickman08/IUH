baseball.wins =
  read.table("baseball-wins.txt",
  header=TRUE)
year1.wins = baseball.wins$year1.wins
year2.wins = baseball.wins$year2.wins
qqnorm(year1.wins)
qqnorm(year2.wins)
plot(year1.wins, year2.wins)
binorm.scatter(cbind(year1.wins,
  year2.wins))
cor(year1.wins, year2.wins)
mean(year1.wins)
mean(year2.wins)
sd(year1.wins)
sd(year2.wins)
lm(year2.wins ~ year1.wins)
plot(year1.wins, year2.wins)
abline(lm(year2.wins ~ year1.wins),
  col="red")
# 2015: ARI had 79 wins
# Prediction for 2016
39.0826 + 0.5173 * 79
# 2015: STL had 100 wins
# Prediction for 2016
39.0826 + 0.5173 * 100
# 2015: PHI had 63 wins
# Prediction for 2016
39.0826 + 0.5173 * 63
# Compare to y=x
abline(0, 1, col="blue")
# Slope isn't 1 becase of
# regression toward the mean
# Estimated slope
slope = cor(year1.wins, year2.wins) *
  sd(year2.wins)/sd(year1.wins)
# Estimated intercept
mean(year2.wins) - slope *
  mean(year1.wins)
summary(lm(year2.wins ~ year1.wins))
# According to the bivariate normal,
# what's the prob. a team that won
# 63 games last year will win at least
# 81 games this year?
mean63 = 39.0826 + 0.5173 * 63
rse = 9.77
1 - pnorm(81, mean63, rse)
