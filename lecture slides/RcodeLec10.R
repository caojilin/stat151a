rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")



#Lecture Eleven - Confidence and prediction intervals
library(faraway)
data(savings)
names(savings)
help(savings)

g = lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(g)


#Confidence Intervals 
#Confidence interval for beta_pop75
summary(g)
qt(0.975, 45)
#C.I is given by 
c(-1.6914977 - 2.014103*1.0835989, -1.6914977 + 2.014103*1.0835989)
#Contains zero. This implies that the t-test for beta_pop75 = 0 will have p-value larger than 0.05. 

#Confidence interval for ddpi
summary(g)
c(0.4096949 - 2.014103*0.1961971, 0.4096949 + 2.014103*0.1961971)
#This interval does not contain zero so we reject the null hypothesis beta_ddpi = 0. 
#However, this interval is very wide in the sense that the upper limit is about 55 times the lower limit. This means that we are not really confident about the exact effect of ddpi on savings, even though it is statistically significant. 

#Confidence intervals for all betas can be obtained by 
confint(g)

#Prediction Intervals
#Consider the body fat data: 
body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
md = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT, data = body)
summary(md)

#Suppose we want to predict the Body Fat Percentage for a 30 year old man who is 70 inches tall and 180 pounds in weight. 

#Fit a linear model: 
md = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT, data = body)
summary(md)
x0 = c(1, 30, 180, 70)
#predicted response
pred.res = sum(x0*coef(md))
pred.res

#Prediction intervals
#t-critical value
tcr = qt(0.975, 248)
cov.beta = vcov(md) #vcov gives sigma^2(X^T X)^(-1)
#Width of the confidence interval 
cw = tcr * sqrt(t(x0) %*% cov.beta %*% x0)
#C.I for the mean response
c(pred.res - cw, pred.res + cw)

#Prediction interval for the response
#(residual standard error is 5.809)
xmat = model.matrix(md)
p2w = tcr * (5.809) * sqrt(1 + t(x0) %*% solve(t(xmat)%*%xmat) %*% x0)
p2w
c(pred.res - p2w, pred.res + p2w)

#Using the predict function
x0 = data.frame(AGE = 30, WEIGHT = 180, HEIGHT = 70)
predict(md, x0, interval = "confidence")
predict(md, x0, interval = "prediction")