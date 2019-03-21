rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")

#Lecture Five 
#Illustrating the Gauss-Markov Theorem
#Simulated Data
#True regression line:
beta0 = 32
beta1 = 0.5
plot(seq(59, 76, 0.01), beta0 + beta1*seq(59, 76, 0.01), type = "l", ylim = c(45, 90), xlab = "x", ylab = "y", col = "red")
#Consider generating regression data of length n = 100 from this line M = 2000. Each time, we estimate the true regression line by the least squares estimator and another adhoc linear unbiased estimator. We can then compare the accuracy of the least squares line with the other line. 

#The variablity of the least squares line: 
n = 100
x = seq(59, 76, length.out = n)
M = 2000
sig = 6
m = 20
for(i in 1:M)
{
  y = rnorm(n, beta0+beta1*x, sig)
  abline(lm(y~x))
  rs = sample(n)[1:m]
  abline(lm(y[rs] ~ x[rs]), col = "green")
}
abline(c(beta0, beta1), col = "red")