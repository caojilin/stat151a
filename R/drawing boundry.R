library(MASS)
library(arm)
library(PRROC)
library(caret)
set.seed(2)

# multivariate normal data generating 
# first experiment, very imbalenced two classes datasets
num_class1 = 1000
num_class2 = 20

X = matrix(rep(0, (num_class1 + num_class2)*2), ncol=2)
mu1  = c(-2,-2)
sigma1 = diag(2)
mu2  = c(1,1)
sigma2 = matrix(c(1,0,0,2) ,2,2, byrow = T)

for (i in 1:num_class1) {
    obsev = mvrnorm(1, mu1, sigma1)
    X[i,1] = obsev[1]
    X[i,2] = obsev[2]
}
for (i in (num_class1+1):(num_class1+num_class2)) {
  obsev = mvrnorm(1, mu2, sigma2)
  X[i,1] = obsev[1]
  X[i,2] = obsev[2]
}

label = c(rep(0,num_class1), rep(1,num_class2))

plot(X, col=label+2, main = "Original")

logistic.model = glm(label ~ X , family = "binomial")
summary(logistic.model)

y_hat_prob = predict(logistic.model, data.frame(X), type = "response")
y_hat = y_hat_prob > 0.5

mean(y_hat == label)

scores.class0 = y_hat_prob[label == 0]
scores.class1 = y_hat_prob[label == 1]
pr1 = pr.curve(scores.class1, scores.class0, curve = T)
plot(pr1)

#Bayesian Generalized Linear Models.
bayes.model =  bayesglm (label ~ X, family=binomial(link="logit"))
summary(bayes.model)
y_hat = predict(bayes.model, data.frame(X), type = "response")
y_hat = y_hat > 0.5
mean(y_hat == label)

#LDA
#SVM


