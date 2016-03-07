college <- read.csv("College.csv")
fix(college)
rownames(college) <- college[,1]
college <- college[,-1]

# c
pairs(college[,1:10])
plot(college$Private, college$Outstate)
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college,Elite)
summary(Elite)
plot(college$Elite, college$Outstate)

# question 9
Auto <- read.csv("Auto.csv", na.strings = "?")
Auto <- na.omit(Auto)
summary(Auto)
str(Auto)
# Since I think that cylinder and origin myst be factor variables, omit them
(ranges <- sapply(Auto[,-c(2,8,9)], range))
(means <- sapply(Auto[,-c(2,8,9)], mean))
(stds <- sapply(Auto[,-c(2,8,9)], sd))

### ranges, mean and sd without 10th to 85th observations
(ranges_o <- sapply(Auto[-(10:85),-c(2,8,9)], range))
(means_o <- sapply(Auto[-(10:85),-c(2,8,9)], mean))
(stds_o <- sapply(Auto[-(10:85),-c(2,8,9)], sd))

# e
plot(Auto$cylinders, Auto$mpg)
plot(Auto$origin, Auto$mpg)
plot(Auto$weight, Auto$acceleration)
plot(Auto$weight, Auto$displacement)
plot(Auto$weight, Auto$mpg)
plot(Auto$weight, Auto$horsepower)
plot(Auto$acceleration, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
# the easy route
pairs(Auto[,-9]) # name should not be a matter

# question 10
library(MASS)
Boston
pairs(Boston) # not very informative
plot(Boston$indus, Boston$zn)
plot(Boston$nox, Boston$crim)
plot(Boston$black, Boston$zn)
plot(Boston$black, Boston$crim)
plot(Boston$black, Boston$medv)

ind <- which(Boston$medv == min(Boston$medv))
Boston[c(399,406),]
sapply(Boston,range)

(length(which(Boston$rm > 7))) #64 with more than 7
(length(which(Boston$rm > 8))) #13 with more than 8

ind <- which(Boston$rm > 8)
Boston[ind,] # with more than 8 rm