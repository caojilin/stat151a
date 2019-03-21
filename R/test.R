x = women$weight
y = rev(women$height)
plot(x, y)


plot(x, y,xlim=c(100,200),ylim = c(1,100))
abline(a=mean(y), b=0)
abline(lm(y~x))
m1 = lm(y~x)

bb = lm(y ~0+x)
m2 = lm(y~ 0 + x)
abline(a =0, b= bb$coefficients)

beta = bb$coefficients

pred1 = x*m1$coefficients[2] + m1$coefficients[1]
cor(pred1,y)^2
summary(m1)

rss = sum((pred1 - y)^2)
tss = sum((y - mean(y))^2)
1- rss/tss

pred2 = x*beta
# cor(pred2,y)^2
# summary(bb)

rss = sum((pred2 - y)^2)
tss = sum((y - mean(y))^2)
1- rss/tss



x1 = 1:10+rnorm(10, 0, 2)
x2 = seq(50, 100, length.out=10)+rnorm(10, 0, 2)
x3 = seq(20,59, length.out = 10) + rnorm(10, 0, 2)
e = rnorm(10, 0, 3)
b1 = 3
b2 = 5
b3 = 7

y = b1*x1 + b2*x2 + b3*x3+ e


plot(y, x1)
plot(y, x2)

mod = lm(y ~ x1+x2+x3)
summary(mod)
x = model.matrix(mod)
hat = x %*% solve(t(x) %*% x) %*% t(x)
