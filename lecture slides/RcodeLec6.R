rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")


#Lecture Six (Regression terminology, fitted values, residuals etc.)
#When X^T X is not invertible. 
#Consider the body fat dataset
body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
body=read.csv("BodyFat.csv")
head(body, 10)
#Let us fit a linear model for BODYFAT using the explanatory variables AGE, HEIGHT, WEIGHT and THIGH. 
# lm1 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)
lm1 = lm(bodyfat ~ Age + Weight + Height + Thigh, data = body)

summary(lm1)
xmat = matrix(0, nrow(body), 5)
xmat[,1] = rep(1, nrow(body))
xmat[,2] = body$Age
xmat[,3] = body$Weight
xmat[,4] = body$Height
xmat[,5] = body$Thigh
H = xmat %*% (solve(t(xmat) %*% xmat))%*%t(xmat)

yvec = body$bodyfat
#the estimate of the beta vector is given by 
bhat = (solve(t(xmat) %*% xmat))%*%t(xmat)%*%yvec
cbind(bhat, lm1$coefficients)

#Now let us change the X matrix so that it does not have full column rank. 
lm2 = lm(BODYFAT ~ AGE + WEIGHT + I(AGE + WEIGHT) + HEIGHT + THIGH, data = body)
summary(lm2)
#Note how R flags the fact that there is a singularity. 
#In this model is beta1 estimable? beta2? beta3?
#It is easy to see that all three of beta1, beta2 and beta3 are not estimable here. But beta1 + beta3 and beta2 + beta3 are estimable (and beta0, beta4, beta5 are estimable). The estimates of beta1 + beta3 and beta2 + beta3 can be read off from the output of lm1. 

#Regression plane
body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
head(body, 10)
library(scatterplot3d)
s3d = scatterplot3d(body$HEIGHT, body$WEIGHT, body$BODYFAT, main = "3D Plot", type = "n")
lm1 = lm(BODYFAT ~ HEIGHT + WEIGHT, data = body)
s3d$plane3d(lm1)

#Hat Matrix
mod1 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)
summary(mod1)
names(mod1)

#Hat matrix
n = nrow(body)
xmat = matrix(0, nrow(body), 5)
xmat[,1] = rep(1, nrow(body))
xmat[,2] = body$AGE
xmat[,3] = body$WEIGHT
xmat[,4] = body$HEIGHT
xmat[,5] = body$THIGH
H = xmat %*% (solve(t(xmat) %*% xmat))%*%t(xmat)
H.diag = influence(mod1)$hat
cbind(diag(H), H.diag)

#Fitted values: 
#These are simply H %*% Y
mod1 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)
summary(mod1)
names(mod1)
mod1$fitted.values
yvec = body$BODYFAT
fval = H %*% yvec
cbind(mod1$fitted.values, fval)

#Residuals 
names(mod1)
mod1$residuals
cbind(mod1$residuals, yvec - mod1$fitted.values)

#Residual Degrees of Freedom: this is n - p - 1. 
mod1$df.residual

