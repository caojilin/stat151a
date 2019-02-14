#setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")
#Load the Pearson Father-Son Height data.
height = read.table("PearsonHeightData.txt", header = T)
dim(height)
head(height)
plot(Son ~ Father, data = height)

#The regression line
lmod = lm(Son ~ Father, data = height)
abline(lmod, col = "blue")
summary(lmod)

n = nrow(height)
sx = sqrt((sum((height$Father - mean(height$Father))^2))/n)
sy = sqrt((sum((height$Son - mean(height$Son))^2))/n)
r = (sum(((height$Son - mean(height$Son))/sy)*((height$Father - mean(height$Father))/sx)))/n
b1 = r*sy/sx
b0 = mean(height$Son) - b1*mean(height$Father)
c(b0, b1)
lmod$coefficients

#The SD line.
#This line passes through the mean of the dataset (xbar, ybar) but has slope equal to sy/sx if r > 0 and -sy/sx if r < 0.
sd.slope = sy/sx
sd.intercept = mean(height$Son) - (sy/sx)*mean(height$Father)
plot(Son ~ Father, data = height)
abline(lmod, col = "red")
abline(a = sd.intercept, b = sd.slope, col = "blue")

#Example Two: Wages Data
load("wage1.Rdata")
ls()
wages = data
wages.desc = desc

head(wages)
dim(wages)
wages.desc

#To illustrate simple linear regression, we take y = wages$wage and x = wages$educ.

y = wages$wage
x = wages$educ
plot(x, y, xlab = "Education", ylab = "Hourly Wages")
#The regression line
slm = lm(wage ~ educ, data = wages)
summary(slm)
abline(slm, col = "red")
#The estimated intercept term is -0.90485. This means that the estimated average hourly wage for people with no education is -0.90485. The fact that this is negative seems nonsensical. Why is this happening? 
#The estimate for beta1 is 0.54136. The interpretation is that for every additional year of education, the average hourly wage increases by 54 cents. Or, for every additional four years of education, the average hourly wage increases by 4*54 = 2.16 dollars. Is this satisfying? A percentage increase in wage is more meaningful than this. For this, we can take y = log(wages$wage) instead of wages$wage
y = log(wages$wage)
x = wages$educ
plot(x, y, xlab = "Education", ylab = "Log Hourly Wages")
reg = lm(y ~ x)
summary(reg)
abline(reg)
#The estimate of beta1 ie 0.082744. This means that the average hourly wage increases by 8.3 percent for every additional year of education.


