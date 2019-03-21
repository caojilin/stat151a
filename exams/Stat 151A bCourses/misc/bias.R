library(glmnet)

set.seed(0)
n <- 100
p <- 10
X <- matrix(rnorm(n * p), n, p)
beta <- sample((-10):10, p, replace=T)
trials <- 1e3
beta.hats.OLS <- matrix(0, trials, p)
beta.hats.ridge <- matrix(0, trials, p)
beta.hats.LASSO <- matrix(0, trials, p)


for (t in 1:trials) {
  # draw y many times, compute estimates for each y
  y <- rnorm(n, mean=X %*% beta)
  beta.hats.OLS[t, ] <- coef(lm(y ~ X + 0))
  fit.LASSO <- glmnet(X, y, intercept=F)
  beta.hats.LASSO[t, ] <- coef(fit.LASSO, s=0.8)[-1] # lambda chosen arbitrarily, but fixed
  fit.ridge <- glmnet(X, y, intercept=F, alpha=0)
  beta.hats.ridge[t, ] <- coef(fit.ridge, s=0.6)[-1] # lambda chosen arbitrarily, but fixed
}

# average all the estimates
beta.hats.OLS.ave <- apply(beta.hats.OLS, MARGIN=2, FUN=mean)
beta.hats.ridge.ave <- apply(beta.hats.ridge, MARGIN=2, FUN=mean)
beta.hats.LASSO.ave <- apply(beta.hats.LASSO, MARGIN=2, FUN=mean)

beta.hats.LASSO.ave # biased towards zero
beta.hats.ridge.ave # biased towards zero
beta.hats.OLS.ave # close to true beta
beta