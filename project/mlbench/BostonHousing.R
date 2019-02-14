library(mlbench)
library(car)
library(leaps)
data(BostonHousing)
help(BostonHousing)
bh = BostonHousing

pairs(bh)
#related variables
full = lm(medv ~ . , data=bh)
# crPlots(full)
crPlot(model = full,variable = "lstat")
summary(full)
#rm, lstat, and ptratio are the most significant ones
par(mfrow = c(2, 2))
plot(full, which=1:4)
# the “Residual vs. Fitted” and “Scale-Location” plots appear to have a slight Ushape

#Adjusted R2
b<-regsubsets(medv ~ .,data=bh)
rs = summary(b)
#selected coefficients
coe = rs$which[which(rs$adjr2 == max(rs$adjr2)),]
rsmodel = lm(medv ~ .,  data = bh[coe[-1]])
summary(rsmodel)
#Mallow's cp
coe2 = rs$which[which(rs$cp == min(rs$cp)),]
cpmodel = lm(medv ~ .,  data = bh[coe2[-1]])
summary(cpmodel)
#BIC
coe3 = rs$which[which(rs$bic == min(rs$bic)),]
bicmodel = lm(medv ~ .,  data = bh[coe3[-1]])
summary(bicmodel)
#AIC
step(full)
aicmodel = lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
                   tax + ptratio + b + lstat, data = bh)
summary(aicmodel)

#cross-validation pick the best model # PRESS
sum((rsmodel$residuals/(1- influence(rsmodel)$hat))^2)
sum((cpmodel$residuals/(1- influence(cpmodel)$hat))^2)
sum((bicmodel$residuals/(1- influence(bicmodel)$hat))^2)
sum((aicmodel$residuals/(1- influence(aicmodel)$hat))^2)

#we choose AIC model, which gives the smallest PRESS
#dropped indus and age because of collinearity

correlation_matrix = data.frame(cor(model.matrix(full)))


#Variable transformation part.
#From crPlots, lstat seems to have a quadratic relationship with medv
#also qqplot denotes residuals are right-skewess, we can do log transformation

skewness(bh$medv)

newmodel = lm(formula = medv ~ crim + zn + chas + nox + rm + dis + rad + 
                tax + ptratio + b + poly(lstat,2), data = bh)
#R-squared value improved from 0.7406 to 0.7868 after adding the polynomial term
summary(newmodel)

par(mfrow=c(2,2))
plot(newmodel,which = 1:4)


#best subset selection is performed one last time to determine the optimal model size
regsubs.full=regsubsets(log(medv) ~ crim + zn + chas + nox + rm + dis + rad + 
                          tax + ptratio + b + poly(lstat,2), data = bh)
rs=summary(regsubs.full)

par(mfrow=c(1,2))
plot(rs$adjr2 ,xlab="Number of Variables ", ylab="Adjusted R^2",type="l")
which.max(rs$adjr2)
points(x=8,y=rs$adjr2[8], col="red",cex=2,pch=20)
plot(rs$cp, xlab="Number of Variables ",ylab="Cp", type="l")
which.min(rs$cp)
points(8,rs$cp[8], col="red", cex=2, pch=20)

bestmodel = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                 tax + ptratio + b + poly(lstat,2), data = bh)
summary(bestmodel)
par(mfrow=c(2,2))
plot(bestmodel,which = 1:4)


#Cook’s Distance measures how much the regression would change if a
#point were deleted. Cook’s distance is increased by leverage AND by large residuals: 
#a point far from the centroid (of points) with a large residual can 
#severely distort the regression.
which(cooks.distance(bestmodel)>0.5)
#381,413,412 don't exceed 0.5 Cook's distance mark, which is the rule-of-thumb boundary in trying to 
#determine if points have significantly large Cook’s Distance
diagnostic(nrow(bh),length(bestmodel$coefficients)-1,bestmodel,0.1837)
#remove 372 373

bestmodel2 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                 tax + ptratio + b + poly(lstat,2), data = bh[-c(372,373),])
summary(bestmodel2)
diagnostic(nrow(bh)-2,length(bestmodel2$coefficients)-1,bestmodel2,0.1772)


bestmodel3 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                  tax + ptratio + b + poly(lstat,2), data = bh[-c(372,373,402,413),])
summary(bestmodel3)
diagnostic(nrow(bh)-4,length(bestmodel3$coefficients)-1,bestmodel3,0.1719)

bestmodel4 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                  tax + ptratio + b + poly(lstat,2), data = bh[-c(372,373,402,413,401),])
summary(bestmodel4)
diagnostic(nrow(bh)-5,length(bestmodel4$coefficients)-1,bestmodel4,0.1693)

bestmodel5 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                  tax + ptratio + b + poly(lstat,2), 
                data = bh[-c(372,373,402,413,401,369,400),])
summary(bestmodel5)
diagnostic(nrow(bh)-7,length(bestmodel5$coefficients)-1,bestmodel5,0.1643)

bestmodel6 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                  tax + ptratio + b + poly(lstat,2), 
                data = bh[-c(372,373,402,413,401,369,400,368,506),])
summary(bestmodel6)
diagnostic(nrow(bh)-9,length(bestmodel6$coefficients)-1,bestmodel6,0.1594)

bestmodel7 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                  tax + ptratio + b + poly(lstat,2), 
                data = bh[-c(372,373,402,413,401,369,400,368,506,410),])
summary(bestmodel7)
diagnostic(nrow(bh)-10,length(bestmodel7$coefficients)-1,bestmodel7,0.157)

bestmodel8 = lm(log(medv) ~ crim + chas + nox + rm + dis + rad + 
                  tax + ptratio + b + poly(lstat,2), 
                data = bh[-c(372,373,402,413,401,369,400,368,506,410,399),])
summary(bestmodel8)
diagnostic(nrow(bh)-11,length(bestmodel8$coefficients)-1,bestmodel8,0.1547)


diagnostic = function(n,p,model,sigma_hat){
  #standardized predicted residual
  e_hat = model$residuals
  X = model.matrix(model)
  H = X %*% solve(t(X) %*% X) %*% t(X)
  e_hat_std <- e_hat / (sigma_hat * sqrt(1 - diag(H)))
  e_hat_pred_std = e_hat_std * sqrt((n-p-2)/(n-p-1-e_hat_std^2))
  #e_hat_pred_std follows t-distribution df = n-p-2
  p_values = 2*(1-pt(abs(e_hat_pred_std),df=n-p-2))
  plot(p_values)
  abline(a=0.05,b=0,col="blue")
  sort(p_values[which(p_values<0.05)])
  #bonferroni correction
  p_values[which(p_values < 0.05/n)]
}

