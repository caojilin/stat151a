

rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")


#Partial Residual Plot (also called component + Residual Plot)
library(faraway)
data(savings)
g = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(g)
plot(savings$pop15, residuals(g) + coef(g)['pop15']*savings$pop15, xlab = "Population under 15", ylab = "Savings (Adjusted)")
abline(0, coef(g)['pop15'])
yy = residuals(g) + coef(g)['pop15']*savings$pop15
xx = savings$pop15
temp = lm(yy~xx)
temp$coefficients
g$coefficients


#Note that the estimated slope coefficient in this simple linear regression equals the estimate of beta_pop15 in the regression g. But the standard error of the slope coefficient is quite different from the standard error corresponding to beta_pop15 in g. 

#We see two groups in the partial residual plot. It suggests that there may be a different relationship in the two groups. We can try to fit two separate regression. 

g1 = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (pop15 > 35))
g2 = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (pop15 < 35))
summary(g1)

summary(g2)


g$coefficients
g1$coefficients
g2$coefficients
#Note that the results are quite different. The graphical analysis has shown a relationship in the data that a purely numerical analysis might easily have missed. 

#Partial Regression Plots
library(faraway)
data(savings)
g = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(g)
#Let us look at the partial regression plot for the savings ratio against pop15. 
d = residuals(lm(sr ~ pop75 + dpi + ddpi, savings))
m = residuals(lm(pop15 ~ pop75 + dpi + ddpi, savings))
plot(m, d, xlab = "pop15 residuals", ylab = "Savings residuals")

g3 = lm(d ~ m)

g3$coefficients
g$coefficients  
abline(0, coef(g)['pop15'])
#Notice how the slope in the plot and the estimated coefficient for pop15 in the original regression are the same.

#Checking homoskedasticity (constant variance), normality and uncorrelatedness of the errors 
#1a) Checking Assumption of constant variance
#Plot residuals against the fitted values. 
#Some examples of residuals against fitted values plots
n = 200
xx = 3 + 4*abs(rnorm(n))
yy1 = -2 + 5*xx + rnorm(n)
yy2 = -2 + 5*xx + (xx^(1.5))*rnorm(n)
yy3 = -2 + 0.5*xx^(1.85) + rnorm(n)
mod1 = lm(yy1 ~ xx)
mod2 = lm(yy2 ~ xx)
mod3 = lm(yy3 ~ xx)

par(mfrow = c(1, 3))
{
  plot(mod1$fitted.values, mod1$residuals)
  plot(mod2$fitted.values, mod2$residuals)
  plot(mod3$fitted.values, mod3$residuals)
}

#Savings dataset
library(faraway)
data(savings)
names(savings)
g <- lm(sr ~ ., savings)
plot(g$fitted, g$res, xlab="Fitted", ylab="Residuals")
#This plot does not suggest any non-constant variance or nonlinearity
#One also plots absolute values of residuals against fitted values for checking non-constant variance
plot(g$fitted, abs(g$res), xlab="Fitted", ylab="|Residuals|")
#No evidence of non-constant variance. 

#To get some practice for judging residual plots, consider the following: 
#a) constant variance plots

par(mfrow = c(1, 2)) 
for(i in 1:2) {plot(1:50, rnorm(50))}

#b) strong nonconstant variance

par(mfrow = c(1, 2))
for(i in 1:2) plot(1:50, (1:50)*rnorm(50))

#c) mild nonconstant variance

par(mfrow = c(1, 2))
for(i in 1:2) plot(1:50, sqrt((1:50))*rnorm(50))

#d) Nonlinearity

par(mfrow = c(1, 2))
for(i in 1:2) plot(1:50, cos((1:50)*pi/25) + rnorm(50))

#You may repeat these plots to get an idea of the usual amount of variation. 

#Now let us look at some residuals against explanatory variable plots. 
plot(savings$pop15, residuals(g), xlab = "population under 15", ylab = "Residuals")

plot(savings$pop75, residuals(g), xlab = "population over 75", ylab = "Residuals")

#From the first plot above, two groups can be seen. Let us compare and test the variances of the residuals in these groups. Given two independent samples from normal distributions, we can test for equal variance using the ratio of the two sample variances as the test statistic. The null distribution is F with degress of freedom given by the two samples. 

var.test(residuals(g)[savings$pop15>35], residuals(g)[savings$pop15<35])
#A significant difference is seen. This suggest nonconstant variance. There are two main approaches to deal with nonconstant variance: (a) weighted least squares (later) and (b) variable transformations. I will illustrate transformations in the next class.

