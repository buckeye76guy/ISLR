# chapter 6 lab
library(ISLR)
View(Hitters)
names(Hitters)
# some salary terms missing
dim(Hitters)
sum(is.na(Hitters$Salary)) # I would personally do: length(which(is.na(Hitters$Salary)))
## get rid of NAs
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

# The regsubsets() function is part of the leaps library
library(leaps) # to have access to regsubsets(): best subset selection, either one, forward, back...
# check out the vcov() function... its like (not really) the coef() function
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full) # asterisks means that the variable is included in the subset.
# nvmax to specify max variables subset, by default, we get up to 8-variable model. page 245 ISLR
regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19) # I tried nvmax > 19 and no errors.
# it had no effect, it used the 19 max!
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq
# plotting RSS and adjR^2
par(mfrow=c(1,2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq", type = "l")
#par(mfrow=c(1,1)) # if this is here, the last plot would no longer be available for us to draw
# points on it as follows
which.max(reg.summary$adjr2) #11
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)
#par(mfrow = c(1,1)) # since this is commented out, the previous: 1 row,2 columns still holds

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp) # 10
points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)
which.min(reg.summary$bic) # 6
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)

# using regsubsets() built-in plot() command: ?plot.regsubsets
par(mfrow = c(1,1)) # each plot is too large to have in same rows
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")
plot(regfit.full, scale = "bic")
# note how the scales are values we could get from the summary()
coef(regfit.full, 6) # to get coefficients with the best 6-variable model
# see page 247 for how we decided to use this 6 variable model.
# It seems only when we want coef() does it actually run the model with those variables. check again

## Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
summary(regfit.bwd)
# you could use these summaries to determine which variable is least slected accros all models
# for example:
reg.summary$which[1,] # returns a vector of TRUE/FALSE for the best 1-variable model. So when you do:
which(reg.summary$which[1,]) # you get a vector of indices that can indicate which variable was used.
# If you included an intercept in the model, then the first index corresponds to the intercept since
# it would be true b/c it was selected in the model. so here I get c(1, 13) for the indices of
# variables included in the best 1-variable model. To get the name of the variable, I must do:
names(Hitters)[12] # since 13-1 = 12 to adjust for the existence of the intercept: you get "CRBI"
# The best part is that which(reg.summary$which[1,]) is of class integer:
class(which(reg.summary$which[1,])) ## You can use the names() function of an "integer" object. It turns
# our the names() works on nueric (or any) vectors too, you just can't use the '$' operator!
names(which(reg.summary$which[1,])) ## names works and you get: "(Intercept)" "CRBI"
names(which(reg.summary$which[1,]))[-1] ## will give all variables used except the intercept.
# Just be cautious when intercept is not included in model, then there is no need to exclude 1st value
names(which(reg.summary$which[10,]))[-1] # gives you the variables used in the best 10-variable model
# using this technique, you can easily count which variables were used the most or the least!!!!

# The best 7-variable models identified by best subset selection, forward stepwise selection and bacward
# stepwise selection are different
coef(regfit.full,7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7) # of course one can simply get the name of the variables used as I did above and see
# if the vectors are identical! I can automate that process from what I discovered

## Choosing among models using the validation set approach and cross-validation
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep = TRUE) # remember that you could use the 'prob ='
# argument to get a higher number of TRUE if you want...
test <- !train
regfit.best <- regsubsets(Salary~., data = Hitters[train,], nvmax = 19) # notice that you could use the 
# 'subset = ' argument as we do usual lm() functions. it works the same
test.mat <- model.matrix(Salary~., data = Hitters[test,])
# to understand what this model.matrix is doing, view both test.mat and View(Hitters[test,]) at the same
# time. You can see that test.mat has all the predicting variables, the same observations but the
# response variable is missing. It also providex a column for variale called (intercept) and set
# all intercepts to 1 which makes sense since the formula provided allows for intercept. In fact,
# when you do: Salary~. + 0, the intercept column vanishes.
# Also, all string entries have been turned into factors, or coded as 0,1
# it may be nice to investigate what would happen on a variable with 3 levels. there is
# a contrasts argument that helps set contrasts.
# when you try: mymat <- model.matrix(Sepal.Length~., data = iris), the Species column is turned into
# a Speciesversicolor column that is 1 for versicolor and 0 for both setosa and virginica. This could
# be a problem if we wanted to have all 3 levels of Species. Trying:
# mymat <- model.matrix(Sepal.Length~., data = iris, contrasts = list(Species = "contr.sum")) had
# different results so did: contr.poly and others... we could always begin by turning Species to factor
val.errors <- rep(NA, 19)
for(i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[,names(coefi)]%*%coefi # you would think that dimensions do not match but they do
  # coefi is a column vector, not a row vector. try 1:4 and t(1:4), the way they print to the
  # scree is different. btw %*% is for matrix
  # multiplication.
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2) # MSE
}
val.errors
which.min(val.errors) # 10 : the best 10-variable has the lowest MSE
coef(regfit.best, 10)
## before we move on, the advantage of this approach over predict(). If we used predict(), we would
# have to do first run a lm() for each model, which means that we would have to get the names of
# the variables per set, then run lots of models, then predict for each model, then perform additional
# stuff. This approach is easy, just do matrix multiplication since the function coef directly
# returns the coefs from the models without producing (not saved in memory) lm() fits everytime!
# MOST IMPORTANTLY: there is NO predict() function for regsubsets(). BUT we can write one to automate
# what we just did:

predict.regsubsets <- function(object, newdata, id, ...){ # the ... is for flexibility, 
  # learn more about it
  form <- as.formula(object$call[[2]]) # the call object is like a list, check out:
  # http://adv-r.had.co.nz/Expressions.html#calls
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[,xvars]%*%coefi
}

# now we perform best subset selection on the full data to get the best 10-variable model since this
# model may have different variables than the one from the training set
regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 10) # only interested in best 10-variable model since previous approach indicated
# a subset of 10 variables worked well on test data

# now, choose among the models of different sizes using cross-validation
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace = T)
cv.errors <- matrix(NA, k, 19, dimnames = list(NULL, paste(1:19))) # 10 rows, 19 columns
# check out: a <- matrix(1:28,7, dimnames = list(paste(1:7))) and
# a <- matrix(1:28,7, dimnames = list(NULL, paste(1:4))) versus simply a <- matrix(1:28,7)
# they print differently to the screen.
for(j in 1:k){
  best.fit <- regsubsets(Salary~., data = Hitters[folds != j,], nvmax = 19) # the elements in jth folds
  # that equal to j are in test set and remainder in training set. That's good, estimate probabilities
  # higher numbers of training observations overall...
  for(i in 1:19){
    pred <- predict(best.fit, newdata = Hitters[folds == j,], id = i) # our own created function
    # note that by calling it predict.regsubsets, what we did was create the predict method
    # for objects of class regsubsets, we could do predict.lm and override the built in method for lm
    # but that only lasts while the session is still open.
    cv.errors[j,i] <- mean((Hitters$Salary[folds == j] - pred)^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean) # don't use sapply, it's a mtrix so you get unexpected stuff
mean.cv.errors
# par(mfrow = c(1,1)) # I do not need this here since I already set it above
plot(mean.cv.errors, type = 'b')
## cross validation suggests that best 11-variable model has lowest cross validation error.
# you can see it from the plot or run: which.min(mean.cv.errors)

# Now again, we try to obtain the best 11-variable model from full data!
reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)

# So use cross validation to get the best n in: best n-variable model. then use full data to get
# the proper subset and more accurate coefficient. Neat

## Lab 2: Ridge regression and the Lasso
# the glmnet package and the glmnet() function
x <- model.matrix(Salary~., Hitters)[,-1] # the -1 excludes the (Intercept) column
y <- Hitters$Salary
## Ridge regression
library(glmnet) # contains functions to create ridge regression and lasso regression models
Grid <- 10^seq(10, -2, length = 100) # a grid of values to represent lambdas to choose from
ridge.mod <- glmnet(x,y, alpha = 0, lambda = Grid) # alpha 0: ridge regression, alphs 1: Lasso
dim(coef(ridge.mod)) # 20 rows, one for each predictor plus an intercept, 100 cols, 1 for each lambda

# comparing l2 norm for coefs when lambda is large vs small
ridge.mod$lambda[50] # the 50th lambda value from the Grid supplied: ~ 11498
coef(ridge.mod)[,50] # The entire 50th column, all coefs for the 50th lambda value. Each entry
# in this column corresponds to a predictor or the intercept
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm of all coefs except for the intercept: ~ 6.36

ridge.mod$lambda[60] # the 50th lambda value from the Grid supplied: ~ 705
coef(ridge.mod)[,60] # The entire 60th column, all coefs for the 60th lambda value. Each entry
# in this column corresponds to a predictor or the intercept
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # l2 norm of all coefs except for the intercept: ~ 57.1
# as one can see, individual coefs must not be compared but the l2 norm of the vector of coefs
# is small when lambda is large compared to when lambda is small.

# we can use predict to get coefficients, fitted values, logits..
# checkout ?predict.glmnet : you can also specify a lambda that was not previously used with the s arg
predict(ridge.mod, s = 50, type = "coefficients")[1:20,]
# without the [1:20,], it would return a (20,1) matrix. with the [1:20,], it returns a col vector.
# s can be a n-vector, i.e s = c(50,60) is valid. but then with the [1:20,] you get a (20,2) matrix
# which is exactly what you get without it. Of course [1:20,1] would return a column vector!

# training and test set
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2) # about half observations into training. remember replace is false
# here. Also sample uses floor(nrow(x)/2) here since nrow(x)/2 is a decimal. sample(1:10, 2.9) : 2-vector
test <- -train # this should make sense
y.test <- y[test]
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = Grid, thresh = 1e-12) # see documentation
# for thresh.
ridge.pred <- predict(ridge.mod, s = 4, newx = x[test,]) # note that now we are predicting fitted values
# so we provide a new argument, the training set. Also, prediction here is with lambda = 4
mean((ridge.pred - y.test)^2) # test MSE ~ 101037
# what we would have gotten had we fit a model with only an intercept
mean((mean(y[train]) - y.test)^2) # ~ 193253
# what we get with a very large lambda
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test,])
mean((ridge.pred - y.test)^2) # ~ 193253

# to check if advantageous to use lambda = 4 or stick with least square
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test,], exact = T) # since lambda is 0, we are asking
# for a least sq fit. In order to have exactly a least sq, we must use the exact = T argument.
mean((ridge.pred - y.test)^2) # ~ 114783
lm(y~x, subset = train) # the coefs that you get match the ones for the line below
predict(ridge.mod, s = 0, exact = T, type = "coefficients")[1:20,] # exact coefs as above

# cross validation to decide the proper lambda: use cv.glmnet() function
# the argument nfolds changes the default 10 folds CV. checkout: ?cv.glmnet
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out) # it has its own plot method
bestlam <- cv.out$lambda.min # bult in to decide best lambda :i.e for low test MSE
bestlam # ~ 212
# let's use this lambda to see how it performs on our test set
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
mean((ridge.pred - y.test)^2) # ~ 96016 test MSE, lower than when lambda = 4

# as is customary, now that we have a cross validated estimate for lambda, we use it on the
# whole dataset to examine coefficient estimates
out <- glmnet(x,y,alpha = 0) # model on whole data set
predict(out, type = "coefficients", s = bestlam)[1:20,]

# The Lasso
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = Grid)
plot(lasso.mod)
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2) # ~ 100743.4

out <- glmnet(x,y, alpha = 1, lambda = Grid) # on the full data
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef != 0] # only 7 of the coefficients (excluding intercept) are non-zero

## PCR and PLS
# Principal Component Regression ## using the pls library
any(is.na(Hitters)) # making sure there are no NA
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV") # will do 10 fold cv for
# different values of M, # of principal components...
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

# now we perform pcr on our training set to see its performance on the test
set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")
pcr.pred <- predict(pcr.fit, x[test,], ncomp = 7) # check out predict.mvr
# remember that the x matrix has names and stuff
mean((pcr.pred - y.test)^2) # ~ 96556
## pcr on full data
pcr.fit <- pcr(y~x, scale = TRUE, ncomp = 7)
summary(pcr.fit)

# Partial Least Squares: the plsr() function
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset = train, scale = TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP") # val.type can be 3 things
pls.pred <- predict(pls.fit, x[test,], ncomp = 2)
mean((pls.pred - y.test)^2) # ~ 101417

## pls using full data
pls.fit <- plsr(Salary~., data = Hitters, scale = T, ncomp = 2)
summary(pls.fit)