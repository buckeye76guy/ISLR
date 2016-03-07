## Exercise question 4_a experiment
myfun <- function(a,x){
  vec <- numeric(0)
  for(el in x){
    if(el >= a - .05 & el <= a + .05){
      vec <- c(vec,el)
    }
  }
  return(length(vec))
} # takes an observation and a vector to determine how many obs in the vector will be used

#set.seed(0)
x <- runif(200) # it seems to me we get something around 10% of the length of x
N <- 0
for(i in 1:1000){
  a <- runif(1) # one observation
  N <- N + myfun(a,x)
}
print(N/1000)

## experiment for part b
myfun <- function(a,x){ # a is a 2-vector and x a 2-column matrix
  N <- 0
  for(i in 1:nrow(x)){
    el <- x[i,]
    if((el[1] >= a[1] - .05 & el[1] <= a[1] + .05) & (el[2] >= a[2] - .05 & el[2] <= a[2] + .05)){
      N <- N + 1
    }
  }
  return(N)
}
x <- data.frame(x1 = runif(100), x2 = runif(100))
N <- 0
for(i in 1:1000){
  a <- c(runif(1),runif(1)) # 2 observations
  N <- N + myfun(a,x)
}
print(N/1000) # it does look like we get close to 1% of the # of rows od x