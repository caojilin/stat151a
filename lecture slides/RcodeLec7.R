rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")

#Lecture Seven (Residual Sum of Squares, Multiple Rsquared, Standard Errors, Standardized or Studentized Residuals)
body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
head(body, 10)
mod1 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)
summary(mod1)

#Residual Sum of Squares: This is just the sum of squares of residuals:
sum(mod1$residuals^2)
#Decreases when more explanatory variables are added in the model: 
mod3 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH + WRIST, data = body)
sum(mod3$residuals^2)
#Increases when explanatory variables are removed from the model: 
mod4 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT, data = body)
sum(mod4$residuals^2)

#Residual sum of squares can be got using the function deviance. 
deviance(mod1)
sum(mod1$residuals^2)

#R-squared or the Coefficient of Determination
names(summary(mod1))
summary(mod1)$r.squared
#Manual Calculation
yvec = body$BODYFAT
tss = sum((yvec - mean(yvec))^2)
rss = sum(mod1$residuals^2)
rsq = 1 - (rss/tss)
rsq 
summary(mod1)$r.squared
#R-squared increases when more explanatory variables are added to the model
mod3 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH + WRIST, data = body)
summary(mod3)$r.squared
#R-squared decreases when explanatory variables are removed from the model: 
mod4 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT, data = body)
summary(mod4)$r.squared

#Estimate of sigma: This is the square root of RSS/residual.df
rse = sqrt(deviance(mod1)/mod1$df.residual)
rse
#This is exactly the Residual Standard Error reported in 
summary(mod1)
#Residual Standard Error is useful for (a) obtaining standard errors for betahats, (b) assessing the size of residuals, etc. 

#Standard Errors of betahats
xmat = matrix(0, nrow(body), 5)
xmat[,1] = rep(1, nrow(body))
xmat[,2] = body$AGE
xmat[,3] = body$WEIGHT
xmat[,4] = body$HEIGHT
xmat[,5] = body$THIGH

rse*sqrt(diag(solve(t(xmat) %*% xmat)))
#These values can be found from 
summary(mod1)





