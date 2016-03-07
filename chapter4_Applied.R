# chapter 4 Applied homework
myfun <- function(x,n=-1){ # won't take care of errors
  # takes a sq matrix and zeros out the lower diagonal entries if n < 0 and upper entries if n >= 0
  if(n < 0){
    for(i in 1:(nrow(x)-1)){
      x[(i+1):nrow(x),i] <- 0
    }
    return(x)
  } else{
    return(t(myfun(t(x))))
  }
}
library(ISLR) # data
library(MASS) # LDA & co
library(class) # KNN
#a
names(Weekly)
str(Weekly)
pairs(Weekly)
contrasts(Weekly$Direction)
plot(Weekly$Lag5, Weekly$Direction) # tried from Lag1 to Lag5 and no true patterns
plot(Weekly$Volume, Weekly$Direction) # no definite patterns, some volumes have now down direction tho
round(myfun(cor(Weekly[,-9])), 2) # remove lower entries and round to 2 decimals
# we can see that only year and volume have a high positive correlation coef. Everything else is small
# the signs of the coefs are good indicators too.

#b
length(which(Weekly$Direction == "Down")) # 484 weeks down
length(which(Weekly$Direction == "Up")) # 605 weeks up
lm_fit <- glm(Direction~., data = Weekly[,-c(1,8)], family = binomial)
summary(lm_fit)
# It seems only Lag2 is statistically significant but only to .05 level

#c
lm.probs <- predict(lm_fit, type="response")
lm.preds <- rep("Down", nrow(Weekly))
lm.preds[lm.probs > .5] <- "Up"
table(lm.preds, Direction = Weekly$Direction) # dia entries: 54 and 557
(54+557)/nrow(Weekly) # overall fraction of correct prediction: ~ 56%
54/484 # proportion of downs that were correctly identified ~ 11%
557/605 # proportion of ups correctly identified ~ 92%

#d
train <- Weekly$Year < 2009 # to retain observations from 1990 to 2008
lm_fit1 <- glm(Direction~Lag2, data = Weekly, family = binomial, subset = train)
lm.probs1 <- predict(lm_fit1, newdata = Weekly[!train,], type="response")
lm.preds1 <- rep("Down", length(lm.probs1))
lm.preds1[lm.probs1 > .5] <- "Up"
table(lm.preds1, Direction=Weekly$Direction[!train]) # diag entries 9 and 56
mean(lm.preds1 == Weekly$Direction[!train]) # proportion of correct ~ 62.5%

#e
lda_fit <- lda(Direction~Lag2, data = Weekly, subset = train)
lda.class <- predict(lda_fit, newdata = Weekly[!train,])$class
table(lda.class, Direction=Weekly$Direction[!train]) # same confusing table as the logistic
mean(lda.class == Weekly$Direction[!train]) # ~ 62.5%

#f
qda_fit <- qda(Direction~Lag2, data = Weekly, subset = train)
qda.class <- predict(qda_fit, newdata = Weekly[!train,])$class
table(qda.class, Direction=Weekly$Direction[!train]) # diag entries 0 and 61
mean(qda.class == Weekly$Direction[!train]) # ~ 58.7%

#g
train.X <- as.matrix(Weekly$Lag2[train]) # training predictor!! must be a matrix! not a vector!!!
test.X <- as.matrix(Weekly$Lag2[!train]) # test predictor
train.Direction <- as.matrix(Weekly$Direction[train]) # response of training data
set.seed(1) # b/c knn RANDOMLY breaks ties
knn.pred <- knn(train.X, test.X, train.Direction, k = 1) # 1NN predictions
table(knn.pred, Direction = Weekly$Direction[!train]) # diag entries: 21 and 31
mean(knn.pred == Weekly$Direction[!train]) # 50%

#h
# it seems both LDA and the logistic regression perform better on this test data

#i : experiment
set.seed(1)
knn.pred1 <- knn(train.X, test.X, train.Direction, k = 8)
table(knn.pred1, Direction = Weekly$Direction[!train])
mean(knn.pred1 == Weekly$Direction[!train])
# rate of corr_pred: k = 2: 50.96%, k = 3: 54.8%, k = 4: 61.54%, k = 5 or 6 or 7: 53.85%, k = 8: 53.88%

train.X <- as.matrix(Weekly$Today[train])
test.X <- as.matrix(Weekly$Today[!train])
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction = Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])
# Just as I thought, Direction is up when today is positive and down when negative
# k = 1 shows no errors at all. when k becomes 5, minor errors come to light

# since lag2 and lag5 are positively (but very weekly) correlated with Today, I will use them
train.X <- cbind(Weekly$Lag2,Weekly$Lag5)[train,]
test.X <- cbind(Weekly$Lag2,Weekly$Lag5)[!train,]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 4) # it seems k = 2 is optimal
table(knn.pred, Direction = Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])

# interaction
train.X <- cbind(Weekly$Lag2,Weekly$Lag5,Weekly$Lag2*Weekly$Lag5)[train,]
test.X <- cbind(Weekly$Lag2,Weekly$Lag5,Weekly$Lag2*Weekly$Lag5)[!train,]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3) # different k's different results
table(knn.pred, Direction = Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])

train.X <- scale(cbind(Weekly$Lag2,Weekly$Year,Weekly$Lag2*Weekly$Year)[train,])
test.X <- scale(cbind(Weekly$Lag2,Weekly$Year,Weekly$Lag2*Weekly$Year)[!train,])
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 3) # 
table(knn.pred, Direction = Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])

train.X <- scale(cbind(Weekly$Lag2, (Weekly$Lag2)^2, Weekly$Year, (Weekly$Year)^2,
                       Weekly$Lag2*Weekly$Year)[train,])
test.X <- scale(cbind(Weekly$Lag2, (Weekly$Lag2)^2, Weekly$Year, (Weekly$Year)^2,
                      Weekly$Lag2*Weekly$Year)[!train,])
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 8)
table(knn.pred, Direction = Weekly$Direction[!train])
mean(knn.pred == Weekly$Direction[!train])

# logistic, lda and qda with interactions and more
lda_fit <- lda(Direction~poly(Lag2,1), data = Weekly, subset = train)
lda.class <- predict(lda_fit, newdata = Weekly[!train,])$class
table(lda.class, Direction=Weekly$Direction[!train]) 
mean(lda.class == Weekly$Direction[!train]) # ~ 62.5% when power is 1, lower otherwise

qda_fit <- qda(Direction~poly(Lag2,2), data = Weekly, subset = train)
qda.class <- predict(qda_fit, newdata = Weekly[!train,])$class
table(qda.class, Direction=Weekly$Direction[!train]) # diag entries 0 and 61
mean(qda.class == Weekly$Direction[!train]) # ~ 58.7% when poly is 1 or 3, ~ 62.5% with 2

lm.fits <- glm(Direction~poly(Lag2, 3), data = Weekly, family = binomial, subset = train)
summary(lm.fits) # non-linear terms irrelevant: stick with interactions
lm.probs <- predict(lm.fits, newdata = Weekly[!train,], type = "response")
lm.preds <- rep("Down", length(lm.probs))
lm.preds[lm.probs > .5] <- "Up"
table(lm.preds, Direction = Weekly$Direction[!train])
mean(lm.preds == Weekly$Direction[!train])

lm.fits <- glm(Direction~Lag5+Volume*Year, data = Weekly, family = binomial, subset = train)
summary(lm.fits) # non-linear terms irrelevant: stick with interactions
lm.probs <- predict(lm.fits, newdata = Weekly[!train,], type = "response")
lm.preds <- rep("Down", length(lm.probs))
lm.preds[lm.probs > .5] <- "Up"
table(lm.preds, Direction = Weekly$Direction[!train])
mean(lm.preds == Weekly$Direction[!train])

# I tried many different interaction terms and nothing siginificant. Lag5*Year was barely significant

#11
#a
names(Auto) # it also turns out no mpg entry == the median so we're ok
# I had a factor() around the ifelse statement but that doesn't work well in the
# correlation matrix b/c factor is not numeric. I will not remove the factor however!
my_Auto <- data.frame(Auto, mpg01 = factor(ifelse(Auto$mpg < median(Auto$mpg), 0, 1)))

#b
pairs(my_Auto)
boxplot(year~mpg01, data = my_Auto) # try all: accel, weight, hp, disp, cyl seem to matter

#c my test data set may not be gooe enough, only 5 "0" mpg01 || year is also a major factor
# I will take 75% (294 out 392) of the observations to be training and the rest to be test data
train <- c(rep(TRUE, 294), rep(FALSE, 98)) # change train and see
set.seed(1)
train <- rep(F, nrow(my_Auto))
train[sample(1:nrow(my_Auto), 294)] <- T # random

#d
lda.fit <- lda(mpg01~., data = my_Auto[,c(3:6,10)], subset = train)
lda.class <- predict(lda.fit, newdata = my_Auto[!train,c(3:6,10)])$class
table(lda.class, my_Auto$mpg01[!train])
mean(lda.class != my_Auto$mpg01[!train]) # ~ 14.3% test error rate, 
# this model succesfully caught all 5 times mpg01 was 0 and ~85% of 1 right.
# with the random partition, test error rate is ~ 8.2%

#e
qda.fit <- qda(mpg01~., data = my_Auto[,c(3:6,10)], subset = train)
qda.class <- predict(qda.fit, newdata = my_Auto[!train,c(3:6,10)])$class
table(qda.class, my_Auto$mpg01[!train])
mean(qda.class != my_Auto$mpg01[!train]) # ~ 18.4% test error rate
# this model succesfully caught all 5 times mpg01 was 0 and ~81% of 1 right.
# with the random partition, test error rate is ~ 10.2%

#f
lm_fit <- glm(mpg01~., data = my_Auto[,c(3:6,10)], subset = train, family = binomial)
summary(lm_fit)
lm.probs <- predict(lm_fit, newdata = my_Auto[!train,c(3:6,10)], type = "response")
lm.preds <- rep(0, length(lm.probs))
lm.preds[lm.probs > .5] <- 1
table(lm.preds, my_Auto$mpg01[!train])
mean(lm.preds != my_Auto$mpg01[!train]) # ~ 27.6% test error rate
# this model succesfully caught all 5 times mpg01 was 0 and ~71% of 1 right.
# with the random partition, test error rate is ~ 8.2%

#g
train.X <- my_Auto[train,3:6]
test.X <- my_Auto[!train,3:6]
train.mpg01 <- my_Auto$mpg01[train] # i forgot to set seed here before calling knn!
knn.pred <- knn(train.X, test.X, train.mpg01, k = 33)
table(knn.pred, my_Auto$mpg01[!train])
mean(knn.pred != my_Auto$mpg01[!train]) # ~ 24.5% test error rate with k = 1
# ~ 23.5% with k = 5, ~ 20.4% with k = 20

vec <- numeric(0)
for(K in 1:40){
  # i forgot to set seed here before calling knn!
  knn.pred <- knn(train.X, test.X, train.mpg01, k = K)
  vec <- c(vec,mean(knn.pred != my_Auto$mpg01[!train]))
}
which.min(vec) # it seems k = 33 yields ~ 7.1% test error rate! lowest! see confusion matrix
# I would pick knn for this data but it all depends on how we partition the data and which predictors
# we choose, some predictors are barely statistically significant (based on different partitions)

#12 # too easy! I can do by hand

#13
names(Boston)
str(Boston)
any(Boston$crim == median(Boston$crim)) # false
my_Boston <- data.frame(Boston, crim01 = factor(ifelse(Boston$crim < median(Boston$crim), 0, 1)))
# in order for cor to work, all variables must be numeric
round(myfun(cor(data.frame(Boston, crim01 = ifelse(Boston$crim < median(Boston$crim), 0, 1)))), 2)
boxplot(chas~crim01, data = my_Boston)
# after plotting boxplot for variables with difft cor levels, I pick the ones with cor >= .25
ind <- which(abs(round(myfun(cor(data.frame(Boston, 
                                     crim01 = ifelse(Boston$crim < median(Boston$crim), 0, 1)))), 
                2)[,15]) >= .25)[-1] # the variables that matter except crim of course
# I will have different seeds for random train
train <- c(rep(T,380), rep(F,126)) # nrow(my_Boston) is 506
# the train subset above: glm: 7.9% error rate, lda: 11.1% error rate, knn: k = 1, 6.3% error rate
train <- rep(F, 506)
set.seed(2902) # different seeds to have different subsets
train[sample(1:506,380)] <- T # error rates below
# seed => 1:> glm: 8.7%, lda: 14.3%, knn: k = 1, 7.9% error rate
# seed => 2902:> glm: 8.7%, lda: 13.5%, knn: k = 1, 4.8% error rate

#logistic
glm_fit <- glm(crim01~., data = my_Boston[,ind], subset = train, family = binomial)
summary(glm_fit)
glm.probs <- predict(glm_fit, newdata = my_Boston[!train, ind], type = "response")
glm.preds <- rep(0,length(glm.probs))
glm.preds[glm.probs > .5] <- 1
table(glm.preds, my_Boston$crim01[!train])
mean(glm.preds != my_Boston$crim01[!train])

#lda
lda.fit <- lda(crim01~., data = my_Boston[,ind], subset = train)
lda.class <- predict(lda.fit, newdata = my_Boston[!train, ind])$class
table(lda.class, my_Boston$crim01[!train])
mean(lda.class != my_Boston$crim01[!train])

#KNN
train.X <- my_Boston[train, ind[-12]] # b/c 12th variable is crim01
test.X <- my_Boston[!train,ind[-12]]
train.crim01 <- my_Boston$crim01[train]
vec <- numeric(0)
for(K in 1:150){
  set.seed(1)
  knn.pred <- knn(train.X, test.X, train.crim01, k = K)
  vec <- c(vec, mean(knn.pred != my_Boston$crim01[!train]))
}
(index <- which.min(vec))
set.seed(1)
knn.pred <- knn(train.X, test.X, train.crim01, k = index)
table(knn.pred, my_Boston$crim01[!train])
mean(knn.pred != my_Boston$crim01[!train])

# it seems like knn performs much better on this data set, especially when k = 1
# some predictors are irrelevant depending on what subset we use for training.
# it seems the predictors I picked were all relevant at some stage. nox, dis and rad are
# very significant. Based on the coefficients of the logistic regression, nox is very
# important (coef is > 55 on average). black and lstat are barely ever relevant.
# a test error rate of 4% with a highly flexible knn is very good!
# we can predict whether a given suburb has crime rate below the median