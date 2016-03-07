# chapter 5 lab
library(ISLR)
set.seed(1)
train <- sample(392,196) # random arrangement of 196 observations
lm_fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((Auto$mpg - predict(lm_fit, Auto))[-train]^2) # - and not ! b/c train is now a numeric set
# we are looking for the test MSE
## additional commands skipped

# Leave-One-Out-Cross-Validation: LOOCV
glm_fit <- glm(mpg~horsepower, data = Auto)
coef(glm_fit)

lm_fit <- lm(mpg~horsepower, data = Auto)
coef(lm_fit) # glm() without family argument is equivalent to basic lm()

# we will use glm() instead of lm() because glm() works together with the cv.glm() function!!!!
library(boot) # here you can find the cv.glm() function
glm_fit <- glm(mpg~horsepower, data = Auto)
cv.err <- cv.glm(Auto, glm_fit) # we don't need a random seed setter here since we leave out only
# one observation at a time. cv.err return: call, K, delta and seed: this seed is a whole lot of nbers!
cv.err$delta

cv.error <- rep(0,5)
for(i in 1:5){
  glm_fit <- glm(mpg~poly(horsepower,i), data = Auto) # linear to 5th power ploy regression
  cv.error[i] <- cv.glm(Auto, glm_fit)$delta[1] # retain 1sr enrt
}
cv.error

# K-fold cross validation
set.seed(17)
cv.error.10 <- rep(0,10) # this 10 has nothing to do witht the choice of K!
# We could have run the loop for only the 5th poly regression
for(i in 1:10){
  glm_fit <- glm(mpg~poly(horsepower,i), data = Auto)
  cv.error.10[i] <- cv.glm(Auto, glm_fit, K = 10)$delta[1] # CAPITAL K otherwise error!
}
cv.error.10

# Bootstrap
# first create a function, then use the boot() function
alpha.fn <- function(data, index){ # index: set of observations to use to estimate alpha
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y))) # formula 5.7, page 187
}
# quick test
alpha.fn(Portfolio, 1:100) # using all 100 observations
set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace = T)) # now that replace is true,
# we are not necessarily using all 100 observations
boot(Portfolio, alpha.fn, R = 1000) # 1000 bootstraps

# estimating the accuracy of a linear regression model
boot.fn <- function(data, index) return(coef(lm(mpg~horsepower, data = data, subset = index)))
# test
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(392,392, replace = T))
boot.fn(Auto, sample(392,392, replace = T))

# Enters bootstrap approach:: Usually have set.seed() before calling boot()
# since no index is provided below, default of nrow(data) is used
boot(Auto, boot.fn, R = 1000) # note that boot.fn returns a 2-vector! the function passed into
# boot() just has to return a vector, even if the length > 1... all estimates in the $t attr
# so the bootstrap approach gave us an estimate for beta_0 and beta_1 as well as the standar error
# associated with each coefficient. We can also compare that with:
summary(lm(mpg~horsepower, data = Auto))$coef
# we get the same estimates for the coefs but different estimates for the standard errors
# but that is no problem: see page 196
boot.fn <- function(data, index) coefficients(lm(mpg~horsepower+I(horsepower^2), data = data, 
                                                 subset = index))
set.seed(1)
boot(Auto, boot.fn, 1000) # 3rd argument is always R but it does not hurt to specify it!
summary(lm(mpg~horsepower+I(horsepower^2), data = Auto))$coef
# even if you use poly(..,2), the Std errors are relatively closer

## bootstrap: estimate a statistic abouta data, also provides standard error or variability or
## uncertainty about that estimate. So our boot.fn() returns only the coefficients which are the
## statistics of interest here!