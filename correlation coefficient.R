rm(list = ls())

lmod = lm(height ~ weight, women)
summary(lmod)
fitted = lmod$fitted.values
y = women$height
x = women$weight

cor(y, fitted)

lmod2 = lm(mpg ~ disp,data=mtcars)
summary(lmod2)
fitted = lmod2$fitted.values
y = mtcars$mpg
x = mtcars$disp
(-0.041215*sd(x)/sd(y))^2
# = Multiple R-squared

lmod2 = lm(mpg ~ disp+cyl,data=mtcars)
summary(lmod2)
fitted = lmod2$fitted.values
y = mtcars$mpg
x = mtcars$disp
(-0.041215*sd(x)/sd(y))^2
# = Multiple R-squared