# Applied
library(ISLR)
#5a-b
glm_fit <- glm(default~income+balance, data = Default, family = binomial)
train <- c(rep(T, 7500), rep(F, 2500)) # b/c nrow(Default) is 10,000 and 25% of it go to test
length(which(Default$default == "Yes")) # only 333 total observations were default
length(which(Default$default[!train] == "Yes")) # only 79 defaults in the validation set
glm_fit <- glm(default~income+balance, data = Default, family = binomial, subset = train)
glm.probs <- predict(glm_fit, newdata = Default[!train,], type = "response")
glm.preds <- rep("No", 2500)
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, Default$default[!train])
mean(glm.preds != Default$default[!train]) # True is 1 and False is 0... so we're ok
# the test error is .0236 or ~ 2.4%

#c: repeat the process with different training sets. 
# first
train <- c(rep(F, 2500), rep(T, 7500)) # b/c nrow(Default) is 10,000 and 25% of it go to test
length(which(Default$default == "Yes")) # only 333 total observations were default
length(which(Default$default[!train] == "Yes")) # only 91 defaults in the validation set
glm_fit <- glm(default~income+balance, data = Default, family = binomial, subset = train)
glm.probs <- predict(glm_fit, newdata = Default[!train,], type = "response")
glm.preds <- rep("No", 2500)
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, Default$default[!train])
mean(glm.preds != Default$default[!train]) # True is 1 and False is 0... so we're ok
# here test error ~ 2.9%

# second
train <- c(rep(T, 3500), rep(F, 1500), rep(T,4000), rep(F,1000)) 
# b/c nrow(Default) is 10,000 and 25% of it go to test
length(which(Default$default == "Yes")) # only 333 total observations were default
length(which(Default$default[!train] == "Yes")) # only 86 defaults in the validation set
glm_fit <- glm(default~income+balance, data = Default, family = binomial, subset = train)
glm.probs <- predict(glm_fit, newdata = Default[!train,], type = "response")
glm.preds <- rep("No", 2500)
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, Default$default[!train])
mean(glm.preds != Default$default[!train]) # True is 1 and False is 0... so we're ok
# here test error ~ 3.04%

# third : completely different nber of size in sets
set.seed(32312)
train <- sample(c(T,F), 10000, T) # sum(train) show we get 5040 True and the rest False
length(which(Default$default == "Yes")) # only 333 total observations were default
length(which(Default$default[!train] == "Yes")) # now we have 178 defaults in the validation set
glm_fit <- glm(default~income+balance, data = Default, family = binomial, subset = train)
glm.probs <- predict(glm_fit, newdata = Default[!train,], type = "response")
glm.preds <- rep("No", length(glm.probs))
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, Default$default[!train])
mean(glm.preds != Default$default[!train]) # True is 1 and False is 0... so we're ok
# here test error ~ 2.86% but only 58 out 178 correctly identified as defaults
# the test errors are low from my approach but the model is not very satisfactory if you want to
# predict if someone will default or not

#d : first i will create a dummy variable for student
my_Default <- data.frame(Default, student01 = ifelse(Default$student == "No", 0, 1))
set.seed(32312)
train <- sample(c(T,F), 10000, T) # sum(train) show we get 5040 True and the rest False
glm_fit <- glm(default~income+balance+student01, data = my_Default, family = binomial, subset = train)
glm.probs <- predict(glm_fit, newdata = my_Default[!train,], type = "response")
glm.preds <- rep("No", length(glm.probs))
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, my_Default$default[!train]) # could've left Default$default[!train] there
mean(glm.preds != my_Default$default[!train]) # True is 1 and False is 0... so we're ok
# test error here is ~ 2.903%, no real improvement! no major mess up either.
# using a dummy variable does not seem to make a big difference when it comes to test error

#### NOTE: contrasts(Default$student) ### WE DO NOT NEED TO CREATE A DUMMY VARIABLE OR A NEW
#### DATA SET! WE CAN JUST USE THE ORIGINAL DATA WITH THE ORIGINAL STUDENT VARIABLE SINCE IT IS
#### ALREADY CODED AS A FACTOR AND contrasts(Default$student) SHOWS THAT IT IS A DUMMY VARIABLE
#### LET'S TRY IT

set.seed(32312)
train <- sample(c(T,F), 10000, T) # sum(train) show we get 5040 True and the rest False
glm_fit <- glm(default~income+balance+student, data = Default, family = binomial, subset = train)
glm.probs <- predict(glm_fit, newdata = Default[!train,], type = "response")
glm.preds <- rep("No", length(glm.probs))
glm.preds[glm.probs > 0.5] <- "Yes"
table(glm.preds, Default$default[!train])
mean(glm.preds != Default$default[!train]) # True is 1 and False is 0... so we're ok
#### WE GET THE EXACT SAME THING!! SO WE DID USELESS WORK
#### here test error ~ 2.903%

#6
#a : the usual way
set.seed(32312)
train <- sample(c(T,F), 10000, T) # sum(train) show we get 5040 True and the rest False
glm_fit <- glm(default~income+balance, data = Default, family = binomial, subset = train)
summary(glm_fit)$coef # summary(glm_fit)$coef[c("income", "balance"),] for income and balance only
# std error with income is 7.055e-06, for balance: 3.424e-04, for intercept: 6.501e-01

#b : bootstrap
library(boot)
boot.fn <- function(data, index){
  return(coefficients(glm(default~income+balance, data = Default, family = binomial, subset = index)))
  # could do coefficients(glm_fit)[c("income", "balance")] to get only coefs of income and balance
}

#c
set.seed(1) # always set seed before using boot
boot(Default, boot.fn, 1000) # maybe use smaller R when dataset is so large
# std err: intercept: 4.24e-01, income: 4.58e-06, balance: 2.27e-04
# the coefficients estimated aren't too far off from what we got before and the std errors are not
# too far off from what the summary()$coef gives but they are all fairly small.

#d
# due to the way I split the data before I have some fairly different standar errors from either cases

#7
#a
glm_fit <- glm(Direction~Lag1+Lag2, data = Weekly, family = binomial)
summary(glm_fit) # seems only Lag2 is (barely) significant

#b
glm_fit <- glm(Direction~Lag1+Lag2, data = Weekly, family = binomial, 
               subset = c(F,rep(T,nrow(Weekly) - 1)))
summary(glm_fit)

#c
predict(glm_fit, Weekly[1,], type = "response") > .5 # True so prediction: Up
Weekly$Direction[1] # The true observation was Down
# Not correctly classified

#d
# errors <- numeric(nrow(Weekly)) # instead of a for loop, I will use the apply function to see
errors <- sapply(1:nrow(Weekly), function(i){
  ifelse(ifelse(predict(glm(Direction~Lag1+Lag2, data = Weekly[-i,], family = binomial), Weekly[i,], 
          type = "response") > .5, "Up", "Down") == Weekly$Direction[i], 0, 1)}) 
# very neat! proud of myself::: sum(errors) gives 490
### A for loop is substantially faster than the sapply() at times.

#e
mean(errors) # ~ .45
# The LOOC estimate for the test error is .45 or 45%

#8
#a
set.seed(1)
y <- rnorm(100)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100)
# n is 100, p is 2, the equation is: y = beta_0*x + beta_1*x^2 +  where beta_0 = 1, beta_1 = -2

#b
plot(x, y) # There is a clear quadratic relationship between x and y

#c
df <- data.frame(x = x, y = y)
set.seed(101001)
#i
glm_fit <- glm(y~x, data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 5.89   5.888812

#ii
glm_fit <- glm(y~x+I(x^2), data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 1.087   1.086

#iii
glm_fit <- glm(y~x+I(x^2)+I(x^3), data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 1.103   1.102

#iV
glm_fit <- glm(y~x+I(x^2)+I(x^3)+I(x^4), data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 1.115   1.114

#d : repeat stuff from c with different seed
set.seed(202002)
#i
glm_fit <- glm(y~x, data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 5.89   5.888812 : exact same answer as in c

#ii
glm_fit <- glm(y~x+I(x^2), data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 1.087   1.086 : exact same answe as in c

#iii
glm_fit <- glm(y~x+I(x^2)+I(x^3), data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 1.103   1.102 : exact same answe as in c

#iV
glm_fit <- glm(y~x+I(x^2)+I(x^3)+I(x^4), data = df) # using glm because it works well with cv.glm
cv.glm(df, glm_fit)$delta # : 1.115   1.114 : : exact same answe as in c
# Yes we get the same answers as in c, that is because the LOOCV does not randomly partition the
# data set, we only leave one out at a time and each observation serves as a validation set ata point

#e
# the quadratic model had the lowest LOOCV. That is exactly what was expected since the true
# relationship is quadratic

#f
# after going back to part c, I ran the fits and used summary to indicate p-values
# 1st: the linear term is not significant, the intercept was significant
# 2nd: both linear and quadratic term were significant, the intercept was not
# 3rd: both linear and quadratic term were significant, the intercept and cubic term were not
# 4th: both linear and quadratic term were significant, the intercept, cubic and 4th deg term were not
# these agree with the conlcusion drawn based on the LOOCV because we saw that there was a major
# drop in the test error from the linear model to the quadratic one. After that, the test error began
# to rise again suggesting no evidence that higher power terms were needed.
# Even the abscence of the intercept was noticed!

#9
library(MASS)

#a
mu_hat <- mean(Boston$medv) # 22.53

#b : we can estimate std err of sample mean by dividing the sample std dev by the sqrt of # obs
mu_hat_sterr <- sd(Boston$medv)/sqrt(nrow(Boston)) # ~ .40886

#c
set.seed(1)
boot.fn <- function(data, index) return(mean(data[index])) # here data is a vector
(bstrap <- boot(Boston$medv, boot.fn, 1000)) # not large data so R = 1000 is ok
# The standard error is .41 ~ 41% using the bootstrap approach. Very close to what we had in part b
# the mean estimate is also really good

#d : t.test() to test hypotheses and also get confidence interval with: t.test(Boston$medv)$conf.int
# also check out the confint() function: confint(glm_fit), confint(glm_fit, parm = "x") for example
(conf_int <- bstrap$t0 + 2*c(-1,1)*.41) # [mu_hat-2SE(mu_hat), mu_hat+2SE(mu_hat)]
t.test(Boston$medv)$conf.int # very close to the one above. identical to 3 significant digits

#e
(med_hat <- median(Boston$medv))

#f
boot.fn <- function(data, index) return(median(data[index]))
set.seed(1)
(bstrap <- boot(Boston$medv, boot.fn, R = 1000))
# the std.err is 0.38, pretty small error margin compared to the value of the estimate for the median

#g : the way quantile works is fairly intuitive. the probs = seq() is the key.
# note that it is seq() and not c(), so values from 0 to 1 by steps (between 0 and 1)
# step of .25 is just usual quantiles, steps of .1 is deciles, .01 is percentiles and so on
(perc_hat <- quantile(Boston$medv, probs = seq(0, 1, .01))["10%"])
# nice how you can pass in the name of the perc
# or quantile(Boston$medv, probs = seq(0, 1, .01))[11] b/c it starts from the zeroth percentile!
# or easy: quantile(Boston$medv, 0.1), check out: quantile(Boston$medv, c(0.1, .25, .01))
# 10% elements below the first nber, 25% below the second, only 1%  below the 3rd. 
# conclusion: You can just specify the probability you want!!!!

#h
boot.fn <- function(data, index) return(quantile(data[index], .1))
set.seed(1)
(bstrap <- boot(Boston$medv, boot.fn, R = 1000))
# We get a .51 as standard error. Small compared to the value of the estimate