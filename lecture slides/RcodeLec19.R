#Criteria based VARIABLE SELECTION
#Car Seat Position Data
library(faraway)
data(seatpos)
help(seatpos)

names(seatpos)
pairs(seatpos)

g <- lm(hipcenter ~ ., seatpos)
summary(g)

# Criterion-Based Procedure
# If there are p potential predictors, then there are 2^p possible models.
# We fit all these models and choose the best one according to some criterion.
# Some criteria are

# 1. The Akaike Information Criterion (AIC) and the Bayes Information
#    Criterion (BIC) -- We want to  minimize AIC or BIC.

#    -- AIC: -2Log(likelihood) + 2*(1+p)
#    -- BIC: -2Log(likelihood) + (1+p)*log(n)

# For linear regression models, the -2log(likelihood) is replaced by n*Log(RSS/n).
# Larger models will fit better and so have smaller RSS but use more parameters
# Thus the best choice of model will balance fit with model size.
# BIC penalizes larger models more heavily and so will tend to prefer
# smaller models in comparison to AIC.

# We can apply the AIC (and optionally the BIC) to the seat data.
# The R function step() is used below for this purpose. This function does not evaluate the AIC for all possible models but uses a search method that compares models sequentially. Thus
# it bears some comparison to the stepwise methods described previously 
# but with the advantage that no dubious p-values are used i.e., there is no hypothesis testing.

g <- lm(hipcenter ~ ., seatpos)
help(step)
step(g)
n = nrow(seatpos)
n*log(deviance(g)/n) + 2*(1 + 8)
extractAIC(g) 


# The sequence of variable removal is the same as with backward elimination.
step(g, direction = "both") #This also considers adding variables.

n <- nrow(seatpos)
step(g, k=log(n))
#This selection is by BIC. Note that we get a model that is smaller than the one selected by AIC.
n*log(deviance(g)/n) + (log(n))*(1 + 8)
extractAIC(g, k = log(n))
step(g, direction="both", k = log(n))

# 2. Mallow's Cp statistic

#Cp(m) = RSS(m)/(sigma.hat^2) - (n - 2(p(m) + 1))
#  sigma.hat^2 is the estimated variance of errors with all predictors (full model). 
# Note that for the full model, Cp(M) = p + 1
# Cp, adj.R^2, AIC and BIC all trade-off fit in terms of RSS against complexity(p).
# For models of a given size, all the methods above will select the model
# with the smallest RSS.

# Let's first discover the best models for each size:
library(leaps)
help("regsubsets")
b<-regsubsets(hipcenter~.)

# Regsubsets() is a function for subset selection in regression.
# subsets.
rs <- summary(b)
rs$which
rs$cp
rs$adjr2
rs$bic
plot(rs$cp,xlab="No. of Predictors",ylab="Cp Statistic", type = "l")
plot(rs$adjr2,xlab="No. of Predictors",ylab="Adjusted R-squared", type = "l")
plot(rs$bic,xlab="No. of Predictors",ylab="BIC", type = "l")
