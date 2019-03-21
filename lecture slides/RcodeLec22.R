
rm(list = ls())
setwd("G:/My Drive/back up/Linear models/151A 2018 FALL Linear Models/151A 2018 FALL Linear Models")

library(DAAG)
data(frogs)
help(frogs)


# 212 sites of the Snowy Mountain area of New South Wales, Australia were
# surveyed.
# The variables:
#    pres.abs -- 0/1 indicates whether frogs were found.
#    easting  -- reference point
#    northing -- reference point
#    altitude -- altitude in meters
#    distance -- distance in meters to nearest extant population
#    NoOfPools-- number of potential breeding pools
#    NoOfSites-- number of potential breeding sites within a 2 km radius
#    avrain   -- mean rainfall for Spring period
#    meanmin  -- mean minimum Spring temperature
#    meanmax  -- mean maximum Spring temperature

# We wish to explain frog distribution as a function of the other variables. 

plot(northing ~ easting, data=frogs, pch=c(1,16)[frogs$pres.abs+1],
     xlab="Meters east of reference point", ylab="Meters north")

# x- and y- axes indicate the location of the sites. Filled points are
# for sites where frogs were found. Because we are working within a very
# restricted geographic area, we do not expect that the distribution will
# change as a function of latitude and longitude. Let's look at the
# scatterplot for the remaining variables:

pairs(frogs[,4:10], oma=c(2,2,2,2), cex=0.5)
#The scatter plots involving the variables distance and NoOfPools look a little odd. It makes sense to consider transforming them.   
summary(frogs$distance)
plot(density(frogs$distance))
plot(density(log(frogs$distance))) 
summary(frogs$NoOfPools)
plot(density(frogs$NoOfPools))
plot(density(log(frogs$NoOfPools))) 

#Fit the logistic model:
frogs.glm0 <- glm(formula = pres.abs ~ altitude + log(distance) +
                    log(NoOfPools) + NoOfSites + avrain + meanmin + meanmax,
                  family = binomial, data = frogs)
# Default link function for binomial is logit
summary(frogs.glm0)
#We will try to understand all the numbers in the above output starting with the coefficients. 
#Fisher Scoring stands for an algorithm that is a minor modification of Newton's method. Essentially in Fisher Scoring, one replaces the Hessian matrix of the log-likelihood function by its expectation. For logistic regression, the Hessian matrix of the log-likelihood function does not involve y and hence for logistic regression, there is no difference between Newton's method (and IRLS or IWLS) and Fisher Scoring. For other generalized linear models, they might be different. 
names(frogs.glm0)
frogs.glm0$coefficients

