# chapter 5 concept hw
# question 2 g
plot(1:100000, sapply(1:100000, function(i) 1 - (1-1/i)^i))
# h
store <- rep(NA, 10000)
for(i in 1:10000){
  store[i] <- sum(sample(1:100, rep = T) == 4) > 0
}
mean(store) # close to the limit as n goes to infinity