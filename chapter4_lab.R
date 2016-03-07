# chapter 4 lab
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[,-9])
plot(Smarket$Volume)
glm_fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm_fit)
coef(glm_fit)
summary(glm_fit)$coef # This is different from coef(glm_fit) b/c it gives a data frame with the coefs
# and the std errors, z-value and p-value associated with them
summary(glm_fit)$coef[,4] # note that class(summary(glm_fit)$coef) shows that it is a matrix and does
# not have names attribute so we have to subset it using the [,4] to get the column with the pvalues!
# class(summary(glm_fit)$coef[,4]) shows that it is a numeric vector. It just happens that this is a 
# named vector. 
# So you CANNOT DO summary(glm_fit)$coef[,4]$Lag1 but you CAN DO summary(glm_fit)$coef[,4]["Lag1"]
# a vector can't be subset with the '$' operator BUT we do the ["name"] to get the value of interest
contrasts(Smarket$Direction) # shows that Up is 1 and Down is 0
glm.probs <- predict(glm_fit, type = "response")
glm.probs[1:10]
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction = Smarket$Direction) # build a confusion matrix.
# it tallies the observations pairwise in each vector, (1,1), (2,2)... from both vectors
(145+507)/1250 # sum of diagonal entries (correct predictions!) divided by nber of observations
# so we had a correct rate of 52%: can also be computed
mean(glm.pred == Smarket$Direction) # average nber of days we got right
train <- Smarket$Year < 2005 # get the years from 2001 to 2003
Smarket.2005 <- Smarket[!train,] # this will be the 2005 data only: test data
Direction.2005 <- Smarket$Direction[!train] # this is the same as Smarket.2005$Direction
glm_fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial,
               subset = train) # using the subset argument!
glm.probs <- predict(glm_fit, Smarket.2005, type = "response") # Smarket.2005 is the test data
glm.pred <- rep("Down", 252) # b/c test data has 252 observation
glm.pred[glm.probs > .5] <- "Up" # matters that "Up" or "Down" match entries in the Direction vector
# if one is all caps and the other is not, we would have a bad confusion matrix.
table(glm.pred, Direction.2005) # confusion matrix
mean(glm.pred == Direction.2005) # .48
mean(glm.pred != Direction.2005) # .52
### making some small adjustments
glm_fit <- glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm_fit, Smarket.2005, type = "response") # Smarket.2005 is the test data
glm.pred <- rep("Down", 252) # b/c test data has 252 observation
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005) # confusion matrix
mean(glm.pred == Direction.2005) # .56
predict(glm_fit, newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.1,-0.8)), type = "response")

## LDA
library(MASS)
lda_fit <- lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
lda_fit
lda.pred <- predict(lda_fit, Smarket.2005)
names(lda.pred) # "class", "posterior", "x"
lda.class <- lda.pred$class # the class element contains the predictions on the response
table(lda.class, Direction.2005) # almost identical to logistic regression
mean(lda.class == Direction.2005) # proportion of correct predictions: .56 or 56%
sum(lda.pred$posterior[,1] >= .5) # nber of observations with posterior probability of being "Down" at
# least .5. lda.pred$posterior has 2 columns, 1st: "Down", 2nd: "Up" corresponding to the 2 classes
sum(lda.pred$posterior[,1] < .5) # nber of observations with posterior probability of being "Down" < .5
sum(lda.pred$posterior[,1] > .9) # we could also do this. To see 90% certainty of a decrease on a day

## QDA
qda_fit <- qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda_fit
qda.class <- predict(qda_fit, Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class == Direction.2005) # proportion of correct prediction: .60

# K-nearest neighbor
library(class)
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train,] # to retain only the 2001-2004 indices
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,] # the 2005 observations only
train.Direction <- Smarket$Direction[train] # the training response
set.seed(1) # because knn() will break ties RANDOMLY
knn.pred <- knn(train.X, test.X, train.Direction, k = 1) # 1 nearest neighbor
table(knn.pred, Direction.2005) # knn directly returns predicted values so quicker to use
(43+83)/252 # diagonal entries are 43 and 83: .5 (or 50%) correct predictions
## k = 3
knn.pred <- knn(train.X, test.X, train.Direction, k = 3) # 1 nearest neighbor
(MY_TAB <- table(knn.pred, Direction.2005)) # knn directly returns predicted values so quicker to use
(48+87)/252 # diagonal entries are 48 and 87: .54 (or 54%) correct predictions, small improvement
(MY_TAB[1,1] + MY_TAB[2,2])/252 # non-manual way of using the diagonal entries.
# MY_TAB is of class table, not matrix or data frame so it has no name attributes BUT
# MY_TAB["Down","Down"] and MY_TAB["Up","Up"] provide the diagonal entries because the response
# has 2 levels, Down and Up! here the rows correspond to the first entry in the table() so predicted
# values, the columns correspond to the actual values. so MY_TAB["Down","Up"] gives the number of
# observations that were mislabelled as "Down" when they should have been "Up"

# application to the caravan insurance data
dim(Caravan) # dataset in the ISLR package
summary(Caravan$Purchase)
standardized.X <- scale(Caravan[,-86]) # omit the qualitative column
test <- 1:1000
train.X <- standardized.X[-test,] # let train the all but the first 1000 observations
test.X <- standardized.X[test,] # let test be the first 1000 observations
train.Y <- Caravan$Purchase[-test] # the training set's responses
test.Y <- Caravan$Purchase[test] # the test data responses. we will match this with the predicted
set.seed(1) # b/c knn RANDOMLY breaks ties
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(test.Y != knn.pred) # test error rate
mean(test.Y != "No") # proportion of purchases "Yes" in test data set
table(knn.pred, test.Y) # confusion matrix
# we predicted 77 people to buy insurance, only 9 of thse truly did purchase insurance
# still better than random guessing

# k = 3
knn.pred <- knn(train.X, test.X, train.Y, k = 3)
table(knn.pred, test.Y) # of 26 people predicted to purchase, 5 purchased, better

# k = 5
knn.pred <- knn(train.X, test.X, train.Y, k = 5)
table(knn.pred, test.Y) # of 15 people predicted to purchase, 4 purchased, better
### comparing to logistic regression
glm_fit <- glm(Purchase~., data = Caravan, family = binomial, subset = -test) # on train data
glm.probs <- predict(glm_fit, Caravan[test,], type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] <- "Yes" # using .5 as threshold
table(glm.pred, test.Y) # of the 7 people predicted to purchase, ZERO purchased, we got 0 right
# using a different threshold
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes" # using .25 as threshold
table(glm.pred, test.Y) # of the 33 people predicted to purchase, 11 purchased... good