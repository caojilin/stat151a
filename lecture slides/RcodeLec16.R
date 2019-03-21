
rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")

#R code for Lecture Sixteen 
#Regression Diagnostics
#Leverages and detecting unusual observations in regression
#Most of the code below is taken directly from Chapter 4 of Julian Faraway's book on linear models with R. 
#Leverages in the savings dataset:
#Savings dataset
library(faraway)
data(savings)
help(savings)
summary(savings)

plot(sr ~ pop15, data = savings)
plot(sr ~ pop75, data = savings)
plot(sr ~ dpi, data = savings)
plot(sr ~ ddpi, data = savings)

savings[savings$ddpi > 15, ]

savings[savings$dpi > 4000, ]

g = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
summary(g)
plot(influence(g)$hat, type = "h")
abline(h = mean(influence(g)$hat))
abline(h = 2*mean(influence(g)$hat))
savings[order(influence(g)$hat, decreasing = T)[1:2],]
#Libya and the United States have the highest leverages.


#Linear model after removing Libya:
rownames(savings)
g1 = lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings[-49,])
summary(g1)
#g1$coefficients
#The estimated coefficient of ddpi changed quite a bit. 
summary(g)
g$coefficients
g1$coefficients



##Fit without  usa
g2 = lm(sr~pop15 + pop75 + dpi + ddpi, data = savings[-44,])
summary(g2)
g$coefficients
g2$coefficients
#Not much change in the regression output.


#Standardized residuals
ginf = influence(g)
stud = residuals(g)/(summary(g)$sigma*sqrt(1-ginf$hat))
cbind(stud, rstandard(g))
#rstandard gives standarized residuals

#Standardized predicted residuals. These are also called Jackknife residuals
#rstudent gives the Jackknife residuals.
jack = rstudent(g)
jack
jack[which.max(abs(jack))]

#Is Zambia an outlier?Lever
#Compare with the Bonferroni critical value
abs(qt(0.05/(50*2), 44))
#By this, Zambia is not really an outlier. 



#Multiple outliers
#Next is an example of a dataset with multiple outliers. Data are available on the log of the surface temperature and the log of the light intensity of 47 stars in the star cluster CYG OB1. 
data(star)
plot(star$temp, star$light, xlab = "log(Temperature)", ylab = "log(Light Intensity)")
ga = lm(light ~ temp, star)
abline(ga)
range(rstudent(ga))
#No outliers found even though we can see them clearly in the plot. Exclude the four stars on theright. 
ga = lm(light~temp, data = star, subset = (temp > 3.6))
abline(ga, lty = 2)
#This example illustrates the problems with multiple outliers. Two or more outliers can hide each other. 

#Cook's distance
g = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings)
cook = cooks.distance(g)
plot(cook, type = "h")
savings[order(cook, decreasing=T)[1:2],]
#The two countries with maximal Cook's distance are Libya and Japan

g1 = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (cook < max(cook)))
#summary(g1)
g1$coefficients
g$coefficients
#Note that the estimates change quite a bit. For example, the coefficient for ddpi changed by about 50%. 

#We do not like our estimates to be so sensitive to the presence of just one country. To obtain the changes in the coefficient values (of say pop15) after removal of the other countries, we can do the following:
ginf = influence(g)
plot(ginf$coef[,2], ylab = "Change in pop15 coef")
countries = row.names(savings)
#identify(1:50, ginf$coef[,2], countries)
#Note that Japan sticks out on this particular plot. We therefore examine the effect of removing it. 
gj = lm(sr ~ pop15 + pop75 + dpi + ddpi, savings, subset = (countries != "Japan"))
summary(gj)
summary(g)
#Comparing this to the full data fit, we observe several qualitative changes. Notice that the ddpi term is no longer significant and the R^2 value has decreased a lot.

