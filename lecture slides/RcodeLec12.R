rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")


#Bootstrap in Regression:
body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
#Linear Model Fitting
mod = lm(BODYFAT ~ AGE + WEIGHT + HEIGHT + THIGH, data = body)
summary(mod)

#Calculating standard errors for the betas by residual bootstrap. 
ft = mod$fitted.values
resd = mod$residuals

N = 2000
#Let us compute the standard error for the betas by the residual bootstrap. 
b.s = matrix(NA, N, 5)
for(i in 1:N)
{
  new.y = mod$fitted.values + sample(resd, replace = T)#Y^{(j)}
  m.s = lm(new.y ~ body$AGE + body$WEIGHT + body$HEIGHT + body$THIGH)
  b.s[i,] = m.s$coefficients
}
c(sd(b.s[,1]), sd(b.s[,2]), sd(b.s[,3]), sd(b.s[,4]), sd(b.s[,5]))
sqrt(diag(vcov(mod)))

