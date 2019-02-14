rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")

body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
head(body, 10)

#Linear Model Fitting
mod1 = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)
summary(mod1)

#Illustration of t-values and the corresponding p-values
#The t-values reported by R for the estimates of the beta are just the ratio of the estimate and the standard error
summary(mod1)
#The estimate of beta corresponding to the HEIGHT variable is -0.49810
#Its standard error is 0.11313
#The t-value is therefore
-0.49810/0.11313
#which is -4.403
#What is the p-value for the test that the beta corresponding to HEIGHT is zero?
#Even without calculating the p-value, one must observe that the degrees of freedom here
#is quite large (= 247). Hence the t distribution should be close to the standard normal
#distribution. For a normal distribution, the value -4.403 is way out in the tail. So the p-value should be quite small. It can be calculated precisely by
2*(1 - pt(4.403, 247))
#which is a very small value. 
