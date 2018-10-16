### Species diversity by zinc level
# See additional questions from Problem Set 10

# Data:
Background = c(0.76, 1.53, 1.7, 1.89, 1.98, 2.05, 2.2, 2.27)
Low = c(1.4, 1.66, 1.83, 1.88, 2.1, 2.18, 2.38, 2.83)
Medium = c(0.8, 0.98, 1.62, 1.75, 1.94, 2.02, 2.06, 2.1, 2.19)
High = c(0.63, 0.85, 1.04, 1.15, 1.25, 1.37, 1.43, 1.88, 1.9)

# ANOVA from summary statistics
# Grand mean
(8*1.798+8*2.033+9*1.718+9*1.278)/34
# SSB
8*(1.798-1.694471)^2+8*(2.033-1.694471)^2+
9*(1.718-1.694471)^2+9*(1.278-1.694471)^2
# SSW
7*.485^2+7*.445^2+8*.503^2+8*.427^2
# SST
2.568576+6.515454
# Between mean square
2.568576/3
# Within mean square
6.515454/30
# F-statistic
0.856192/0.2171818
# P-value
1-pf(3.942282, df1=3, df2=30)

# Follow-up tests
t.test(Background, Low)
t.test(Background, Low)$p.value
t.test(Background, Medium)$p.value
t.test(Background, High)$p.value
t.test(Low, Medium)$p.value
t.test(Low, High)$p.value
t.test(Medium, High)$p.value
