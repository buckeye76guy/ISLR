# Chapter 3 applied
library(MASS)
library(ISLR)
# question 8
data("Auto")
lm_fit <- lm(mpg~horsepower, data = Auto)
summary(lm_fit)
confint(lm_fit) # this only gives confidence interval for coefficients
# But the following give intervals about uncertainity with the value of the predictor
predict(lm_fit, data.frame(horsepower=98), interval = "confidence")
predict(lm_fit, data.frame(horsepower=98), interval = "prediction")

# plot the response and the predictor, use abline to show model
plot(Auto$horsepower, Auto$mpg)
abline(lm_fit)
par(mfrow=c(2,2))
plot(lm_fit) # Show that the residuals show a pattern, non-linearity
par(mfrow=c(1,1))

# question 9
# a scatterplot matrix with the pairs() function
pairs(Auto)
cor(subset(Auto, select = -name))
mlm_fit <- lm(mpg~.-name, data = Auto) # mlm for multiple lm
summary(mlm_fit)
par(mfrow=c(2,2))
plot(mlm_fit)
par(mfrow=c(1,1))
plot(predict(mlm_fit), rstudent(mlm_fit)) # studentized residuals
plot(hatvalues(lm_fit)) # leverage statistics.
# the residual plot shows some patterns. The studentized residuals show that there are some unusual
# outliers. The leverage plot does show observations with unusual leverage. Here p = 7 and n = 392.
# so (p+1)/n = .02. Any leverage statistics above that means that the observation has high leverage.
# the graph clearly shows a bunch of observations with high leverage.
emlm_fit <- lm(mpg~.-name+year:origin+weight:year:origin, data = Auto) # emlm for expl. multiple lm
summary(emlm_fit) # The interactions I have are significant but every other predictor becomes
# insignificant except weight, year and origin, cylinders are significant up to 0.1 or 90%.
# the Rsquraed hasn't changed much so this model isn't any better really
par(mfrow=c(2,2))
plot(emlm_fit)
par(mfrow=c(1,1)) # the residual plot still shows some patterns
emlm_fit1 <- lm(mpg~log(weight)+sqrt(displacement)+I(origin^2), data = Auto)
summary(emlm_fit1)
par(mfrow=c(2,2))
plot(emlm_fit1)
par(mfrow=c(1,1)) # log of weight and sqrt of displacement are negatively related to mpg.
# origin^2 is barely significant, i.e. p value < .1.

# question 10
data("Carseats")
names(Carseats)
contrasts(Carseats$US)
contrasts(Carseats$Urban) # These 2 matrices show that Yes is 1 and No is 0
lm_fit10 <- lm(Sales~Price+Urban+US, data = Carseats)
summary(lm_fit10)
# interpret coefficients
# intercept: On average, non-Urban non-American stores sell 13,043 for free.
# Price: Holding urban and US status constant, an increase of $1.00 in carseat prices decreases
# the units sold by ~ 54 units (not in thousands).
# UrbanYes: hodling price and US status constant, urban stores sell 21 (not in thousands) less 
# carseats than non-urban stores. The P-Value is insignificant so very irrelevant
# USYes: Holding price and Urban status constant, US stores sell about 1,201 more carseats than non US

# let Y represent sales and X represent price
# Non-Urban American: Y = 13.04 -.05X + 1.2 = 14.24 -.05X
# Non-Urban Non-American: Y = 13.04 -.05X
# Urban American: Y = 13.04 -.05X -.02 + 1.2 = 14.22 - .05X
# Urban Non-American: Y = 13.04 -.05X -.02 = 13.02 - .05X
# Only Price and US are predictors for which we have statistical support to reject H_0

lm_fit10_1 <- lm(Sales~Price+US, data = Carseats)
summary(lm_fit10_1)
# simply based on the summary of the models, the lm_fit10_1 has lower RSE. Both lm_fit10_1 and lm_fit10
# have the same R-sq despite lm_fit10_1 having one less variable. The adjusted R-sq for lm_fit10_1 is
# also better (with one less variable) so lm_fit10_1 fits the data better than lm_fit10

confint(lm_fit10_1) # for the confidence interval of the coefficients
par(mfrow=c(1,2)) # 1 row, 2 columns
plot(predict(lm_fit10_1), rstudent(lm_fit10_1)) # outliers? if rstudent > 3
plot(hatvalues(lm_fit10_1)) # leverage statistics. if any > (p+1)/n = 3/400 = .0075, page 99: hatvalues
par(mfrow=c(1,1)) # The plot shows evidence of high leverage observations. to be sure:
if(any(rstudent(lm_fit10_1) > 3)) print("Evidence of outliers")
if(any(hatvalues(lm_fit10_1) > 3/400)) print("Evidence of high leverage observation")

# question 11
set.seed(1)
x <- rnorm(100)
y <- 2*x+rnorm(100)
mylm_fit <- lm(y~x+0) # could do lm(y~x-1) as well.
summary(mylm_fit)
# The coefficient shows that there is approximately a 1 to 2 relationship between x and y.
# The error (standard deviation) associated with beta is .1065 and can be used to compute its confidenc
# interval. The p-value for the t-statistics is very small ~ 0 so reject H_0 i.e. Beta is not zero
mylm_fit1 <- lm(x~y+0)
summary(mylm_fit1) # The p-value is also significant so we can reject H_0 and say that x ~ .39y
# algebraic proof done
# numerical proof. we have t-statistics of 18.73 for the y~x simple linear regression.
(sqrt(length(y)-1)*sum(x*y))/sqrt(sum(x^2)*sum(y^2) - (sum(x*y))^2) # it works
summary(lm(y~x)) # to find the t-statistics for beta_1 for regression of y onto x
summary(lm(x~y)) # see if they match: They do

# question 12
# The example we have anove has the satisfy question 12, part b
# the follwing exaple will produce Beta_hat that is the same for x~y and y~x
y <- x
set.seed(1)
y[sample(1:100, 35)] <- -y[sample(1:100, 35)] # or simply do y <- sample(x,100)
coef(lm(y~x+0))
coef(lm(x~y+0)) # they are the same up to 2 decimals! That is simply because sum xi^2 = sum yi^2

# question 13
set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = sqrt(.25))
y <- -1 + .5*x + eps # clearly, beta_0 = -1 and beta_1 = .5
coef(lm(y~x)) # not needed, just estimates of beta_0 and beta_1
plot(x,y, col=1) # a linear pattern
slm_fit <- lm(y~x)
summary(slm_fit) # F-statistics is significant and large. small RSE and coeffs are non-zero with 
# great confidence! beta_0_hat and beta_1_hat are very close to beta_0 and beta_1. R-squared is very
# low though. Not much of the variation is explained by the model.
abline(slm_fit, col=2)
abline(a=-1,b=.5,col=3)
legend(x=0, y=0,legend = c("data", "lsq line", "pop line"), col = 1:3, lwd = 1)

lm_fit_13 <- lm(y~x+I(x^2))
summary(lm_fit_13) # There is no evidence that the quadratic term improves the model. pvalue and F-stat
# and the small increase in R^2

set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = sqrt(.05))
y <- -1 + .5*x + eps # clearly, beta_0 = -1 and beta_1 = .5
coef(lm(y~x)) # not needed, just estimates of beta_0 and beta_1
plot(x,y, col=1) # a linear pattern
slm_fit1 <- lm(y~x)
summary(slm_fit1)

set.seed(1)
x <- rnorm(100)
eps <- rnorm(100, mean = 0, sd = sqrt(9))
y <- -1 + .5*x + eps # clearly, beta_0 = -1 and beta_1 = .5
coef(lm(y~x)) # not needed, just estimates of beta_0 and beta_1
plot(x,y, col=1) # a linear pattern
slm_fit2 <- lm(y~x)
summary(slm_fit2)

coef(slm_fit) # original data
coef(slm_fit2) # noisier data
coef(slm_fit1) # less noisi data
# it looks like there needs to be a lot of noise to have a serious shift in beta_1_hat. But the less 
# noisy data presents a better beta_1_hat as it is closer to beta_1.
# The intercept shifts quicker than the slope. The less noisy model is simply better as the estimated
# coefs are much closer to the real coefficients.
confint(slm_fit)
confint(slm_fit1)
confint(slm_fit2) 
# The less noisy data has narrower confidence intervals than the original data which has narrower
# conf intervals than the noisier data.

# question 14
set.seed(1)
x1 <- runif(100)
x2 <- .5*x1+rnorm(100)/10
y <- 2 + 2*x1 + .3*x2 + rnorm(100) # b_0 = 2, b_1 = 2, b_2 = .3
cor(x1,x2)
plot(x1,x2)
lm_fit_14 <- lm(y~x1+x2)
summary(lm_fit_14)
# low RSE but very low Rsq. F-statistics in't very high. b_0_hat is close to b_0. b_1_hat and b_2_ hat
# are not really close to the true values (b_1_hat closer to its true value than b_2_hat).
# We can reject H0: B_1 = 0 but we cannot reject H0: B_2 = 0.
lm_fit_14_1 <- lm(y~x1)
summary(lm_fit_14_1) # we can reject H0: B_1 = 0. This model has low RSE but also very low R square
# it is not much a better fit than the other one but now B_1_hat is much closer to 2!
lm_fit_14_2 <- lm(y~x2)
summary(lm_fit_14_2) # This model has even lower R-square. It is not a better fit and we cannot
# reject H0: b_1 = 0. The results from c-e do not really contradict each other. In the presence of
# x1, x2 is almost irrelevant. In the absence of x1, x2 is relevant in the model.

x1 <- c(x1,.1)
x2 <- c(x2,.8)
y <- c(y, 6)
summary(lm(y~x1+x2)) # now we cannot reject H0: B_1 = 0 but reject H0: B_2 = 0.
summary(lm(y~x1))
summary(lm(y~x2))
rstudent(lm(y~x1+x2))[101] # not an outlier in this model
rstudent(lm(y~x1))[101] # The point is an outlier in this model!
rstudent(lm(y~x2))[101] # not an outlier in this model
hatvalues(lm(y~x1+x2))[101] # (p+1)n = .03
hatvalues(lm(y~x2))[101] # (p+1)/n = .02
hatvalues(lm(y~x2))[101] # (p+1)/n = .02 # it is a high leverage point everywhere! every model

# question 15
data("Boston")
names(Boston)
coefs <- data.frame(matrix(nrow=ncol(Boston)-1, ncol=2))
names(coefs) <- c("Beta_0", "Beta_1")
row.names(coefs) <- names(Boston)[2:14]
Probs <- numeric(0)
for(i in 2:ncol(Boston)){
  coefs[i-1,] <- coef(lm(Boston$crim~Boston[,i]))
  a <- summary(lm(Boston$crim~Boston[,i]))$fstatistic
  p <- pf(a[1], a[2], a[3], lower.tail = F)
  attributes(p) <- NULL
  Probs <- c(Probs, p)
}
names(Boston)[which(Probs >= .1)+1]
# It seems almost every predictor is related to the response except the "chas" variable!
plot(2:14, Probs, ylab = "F-statistics", xlab = "variables: zn to medv") # see the index of the 
# variable with high f statistics
lm_fit_15 <- lm(crim~.,data = Boston)
summary(lm_fit_15)
# statistically significant: zn, nox (barely), dis, rad, black, lstat(barely) and medv
# results from a and b are fairly different, Signs have changed and magnitudes as well
plot(coefs$Beta_1,coef(lm_fit_15)[-1], col=1:13, xlab = "Univariate", ylab = "Multivariate")
legend(x=c(0,25), y=c(-10,-2), legend = names(Boston)[2:14], col = 1:13, ncol = 3, cex = .7,
       pt.cex = 1, pch = 1)
for(i in 2:ncol(Boston)){
  if(names(Boston)[i] == "chas"){
    next()
  }
  print(names(Boston)[i])
  a <- Boston[,i]
  print(summary(lm(Boston$crim~poly(a,3))))
  readline("press enter")
} # it seems only black and chas do not show non-linear patter