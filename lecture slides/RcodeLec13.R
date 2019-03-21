rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")



#Leverage and Mahalanobis Distance
body = read.delim("bodyfat_corrected.txt", header = TRUE, sep = "")
body = read.csv("Bodyfat.csv")
#Linear Model Fitting
mod = lm(bodyfat ~ Age + Weight + Height + Thigh, data = body)
summary(mod)

#design matrix
xmat = model.matrix(mod)
n = nrow(xmat)
p = 4
#The matrix S
S = matrix(0, p , p)
mm = apply(xmat, 2, mean)
for(i in 1:n)
{
  S = S + (xmat[i, -1] - mm[-1])%*%(t(xmat[i, -1] - mm[-1]))
}
S = S/(n-1)
#The Mahalanobis distances are in the vector M.dist
M.dist = rep(NA, n)
for(i in 1:n)
{
  M.dist[i] = (t(xmat[i, -1] - mm[-1])) %*% solve(S) %*% (xmat[i, -1] - mm[-1])
} 
library(MASS)
plot(influence(mod)$hat, M.dist)
abline((1-n)/n, (n-1))

#More on Leverages
sig = matrix(c(10, 1, 1, 0.5), 2, 2)
pc = mvrnorm(n = 100, c(0, 0), sig)
plot(pc[,1], pc[,1], type = "n", xlab = "X1", ylab = "X2")
points(pc[,1], pc[,2])

rgr = lm(pc[,2] ~ pc[,1])
abline(rgr)
summary(rgr)

#What are the leverages here?
help(influence)
influence(rgr)$hat
mean(influence(rgr)$hat)
sort(influence(rgr)$hat)
plot(sort(influence(rgr)$hat), type = "h")

#Add an observation with a large value of pc[,1] (which is the x-variable)
npc = rbind(pc, c(100, 14))
plot(npc[,1], npc[,2])
rgr1 = lm(npc[,2] ~ npc[,1])
abline(rgr1)
abline(rgr, col = "red")
influence(rgr1)$hat
mean(influence(rgr1)$hat)
2/101
sort(influence(rgr1)$hat)
plot(sort(influence(rgr1)$hat), type = "h")
summary(rgr1)

npc = rbind(pc, c(100, 5))
plot(npc[,1], npc[,2])
rgr1 = lm(npc[,2] ~ npc[,1])
abline(rgr1)
summary(rgr1)
abline(rgr, col = "red")
influence(rgr1)$hat
mean(influence(rgr1)$hat)
sort(influence(rgr1)$hat)
plot(sort(influence(rgr1)$hat), type = "h")


#The value of the leverage only depends on the explanatory values
npc = rbind(pc, c(100, 0))
plot(npc[,1], npc[,2])
rgr2 = lm(npc[,2] ~ npc[,1])
abline(rgr2)
abline(rgr, col = "red")
summary(rgr2)
influence(rgr2)$hat
mean(influence(rgr2)$hat)
sort(influence(rgr2)$hat)
plot(sort(influence(rgr2)$hat), type = "h")


npc = rbind(pc, c(100, -14))
plot(npc[,1], npc[,2])
rgr3 = lm(npc[,2] ~ npc[,1])
abline(rgr3)
summary(rgr3)
abline(rgr, col = "red")
influence(rgr3)$hat
mean(influence(rgr3)$hat)
sort(influence(rgr3)$hat)

#Points that are only outliers in y do not have high leverage
npc = rbind(pc, c(0, -14))
plot(npc[,1], npc[,2])
rgr4 = lm(npc[,2] ~ npc[,1])
abline(rgr4)
abline(rgr, col = "red")
influence(rgr4)$hat
mean(influence(rgr4)$hat)
sort(influence(rgr4)$hat)
plot(sort(influence(rgr4)$hat), type = "h")
summary(rgr4)
summary(rgr)
#Now the estimated regression line is not all that different. But the R^2 value is very different. 
