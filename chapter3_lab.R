# chap 3 lab
library(MASS)
library(ISLR)
fix(Boston)
lm_fit <- lm(medv~lstat, data = Boston)
summary(lm_fit)
names(lm_fit)
coef(lm_fit)
confint(lm_fit)
confint(lm_fit, level=.99)
predict(lm_fit, data.frame(lstat=c(5,10,15)), interval = "confidence")
predict(lm_fit, data.frame(lstat=c(5,10,15)), interval = "prediction")
plot(Boston$lstat, Boston$medv)
abline(lm_fit)
par(mfrow=c(2,2))
plot(lm_fit)
dev.off() # either this or par(mfrow=c(1,1)) to stop it from using 4 displays
plot(predict(lm_fit), residuals(lm_fit))
plot(predict(lm_fit), rstudent(lm_fit)) # plotting the residuals against fitted values.
plot(hatvalues(lm_fit))
which.max(hatvalues(lm_fit)) # provides the index of the observation with highest leverage
# turns out this observation had the highest lstat of all, 37.97 when the mean of all lstat is 12.65

# Mutliple regression
lm_fit <- lm(medv~lstat+age, data = Boston)
summary(lm_fit)
# all at once
lm_fit <- lm(medv~.,data=Boston)
summary(lm_fit)
library(car)
vif(lm_fit) # this is a numeric vector but with names. So you can do names(vif(lm_fit)) to get the
# corresponding predictors. You cannot do vif(lm_fit)$crim but you could do vif(lm_fit)["crim"] to
# ge the vif only for the "crim" variable. !!! No $ works on it
lm_fit <- lm(medv~.-age, data = Boston) # the - omit age
summary(lm_fit)

# Interaction terms
lm_fit <- lm(medv~lstat*age, data = Boston)
summary(lm_fit)
lm_fit <- lm(medv~lstat:age, data = Boston) # same as lm(medv~I(lstat*age), data = Boston)
summary(lm_fit)
# You get different results. The lstat:age only provides intercept and lstat times age. See alternative
# formula that I placed next to it. The lstat*age on the other hand provides intercept, coef for lstat,
# coef for age and lstat times age. So a multiple regression and that is why we have different results!

# Non-Linear Transformation of the PREDICTORS!!
lm_fit2 <- lm(medv~lstat+I(lstat^2), data = Boston)
summary(lm_fit2) # large F statistics provide evidence against Ho, i.e. at least one of the predictors
# is related to medv. So Ho was that none were related to it, beta1=beta2=...=0
# The p-value for the F-statistics is also very small so significant so reject Ho.
lm_fit <- lm(medv~lstat, data = Boston)
anova(lm_fit, lm_fit2) # if you flip the orders you will see change in signs. It seems the full model
# is easy to distinguish, DF could be negative if flipped and if Degree of Freedom is negative, 
# the orders of the things are switched. Page 116 near end, how to use table
par(mfrow=c(2,2))
plot(lm_fit2)
par(mfrow=c(1,1))

# Using poly
lm_fit5 <- lm(medv~poly(lstat, 5), data = Boston) #if you try poly power 6, the x^6 term has high pvalue
summary(lm_fit5)

# Log transformation
summary(lm(medv~log(rm), data = Boston))

# Qualitative predictors
data(Carseats)
names(Carseats)
lm_fit <- lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm_fit)
# The contrasts() function returns the coding that R uses for the dummy variables. Page 118
contrasts(Carseats$ShelveLoc) # set sparse=T and the 0s become .
# You could also set your own contrast for the predictor

# Writing Functions. I already know how to write functions but hey I'll do it
LoadLibraries <- function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}