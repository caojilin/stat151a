#R code Lecture 20 


rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")


library(faraway)
data(seatpos)
help(seatpos)


# Cross-Validation - Model Assesment
# Probably the simplest way and most widely used for estimating predicting
# error is cross-validation.  Ideally if we had enough data, we would set
# aside a validation set and use it to assess the performance of our
# prediction. Since data are scarce, this is usually not possible. To finesse
# the problem, K-fold cross-validation uses part of the available data
# to fit the model, and a different part to test it. We split the data into
# K roughly equal-sized parts. For the kth part, we fit the model to the other
# K-1 parts of the data, and calculate the prediction error of the fitted model
# when predicting kth part of the data. We do this for k=1,2,....,K and
# combine the K estimates of prediction error.

# When K= n, we get leave-one-out cross-validation.

#Leave One Out Cross Validation
# Use full model
n <- nrow(seatpos)
pred.y <- rep(NA,n) 
for (i in 1:nrow(seatpos))
{
  g <- lm(hipcenter~., seatpos, subset=(1:n)[-i])
  pred.y[i] <- predict(g, seatpos[i,-9])
}
cv.err.full <-  sum((seatpos[,9]-pred.y)^2)
cv.err.full

#Via Predicted Residuals
#First calculate leverages of the full model
m = lm(hipcenter ~ ., data = seatpos)
m$residuals
lvrg = influence(m)$hat
pred.res = m$residuals/(1 - lvrg)
sum(pred.res^2)

# Use three predictors
for (i in 1:nrow(seatpos))
{
  g <- lm(hipcenter ~ Age+Ht+Leg, seatpos, subset=(1:n)[-i])
  pred.y[i] <- predict(g, seatpos[i,c("Age","Ht","Leg")])
}
cv.err.Age.Ht.Leg <-  sum((seatpos[,9]-pred.y)^2)
cv.err.Age.Ht.Leg

#Using predicted residuals
m = lm(hipcenter ~ Age + Ht + Leg, data = seatpos)
m$residuals
lvrg = influence(m)$hat
pred.res = m$residuals/(1 - lvrg)
sum(pred.res^2)

#Just use Ht: 
m = lm(hipcenter ~ Ht, data = seatpos)
m$residuals
lvrg = influence(m)$hat
pred.res = m$residuals/(1 - lvrg)
sum(pred.res^2)

#Let us pick the models chosen by RSS for each value of p(m) and then compare them via their Leave-One-Out Cross Validation Scores:
library(leaps)
b<-regsubsets(hipcenter~.,seatpos)

# Regsubsets() is a function for subset selection in regression.
# The default search method is exhausive search: search among all possible
# subsets.
rs <- summary(b)
rs$which
m1 = lm(hipcenter ~ Ht, data = seatpos)
m2 = lm(hipcenter ~ Ht + Leg, data = seatpos)
m3 = lm(hipcenter ~ Age + Ht + Leg, data = seatpos)
m4 = lm(hipcenter ~ Age + HtShoes + Thigh + Leg, data = seatpos)
m5 = lm(hipcenter ~ Age + HtShoes + Arm + Thigh + Leg, data = seatpos)
m6 = lm(hipcenter ~ Age + HtShoes + Seated + Arm + Thigh + Leg, data = seatpos)
m7 = lm(hipcenter ~ Age + Weight + HtShoes + Seated + Arm + Thigh + Leg, data = seatpos) 
m8 = lm(hipcenter ~ ., data = seatpos)

cv.scores = rep(-999, 8)
cv.scores[1] = sum((m1$residuals^2)/((1 - influence(m1)$hat)^2))
cv.scores[2] = sum((m2$residuals^2)/((1 - influence(m2)$hat)^2))
cv.scores[3] = sum((m3$residuals^2)/((1 - influence(m3)$hat)^2))
cv.scores[4] = sum((m4$residuals^2)/((1 - influence(m4)$hat)^2))
cv.scores[5] = sum((m5$residuals^2)/((1 - influence(m5)$hat)^2))
cv.scores[6] = sum((m6$residuals^2)/((1 - influence(m6)$hat)^2))
cv.scores[7] = sum((m7$residuals^2)/((1 - influence(m7)$hat)^2))
cv.scores[8] = sum((m8$residuals^2)/((1 - influence(m8)$hat)^2))
cv.scores

plot(cv.scores)
#Therefore the model selected by Leave One Out Cross Validation will have only one explanatory variable which is Ht. 

