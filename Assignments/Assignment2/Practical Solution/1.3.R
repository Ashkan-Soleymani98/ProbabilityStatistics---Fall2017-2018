m <- 1000
a <- 80
b <- 200
X.0 <- 80
randomVariableSequnces <- c(X.0)
temp <- X.0
for(i in 1:1000){
  temp = temp * a
  temp = temp + b
  temp = temp %% m
  randomVariableSequnces <- c(randomVariableSequnces , temp)
}
print(randomVariableSequnces)

f <- function(temp){
  temp = temp * a
  temp = temp + b
  temp = temp %% m
}

#---
tortoise = X.0
hare = f(X.0)
while(tortoise != hare){
  tortoise = f(tortoise)
  hare = f(f(hare))
}
#---
cycle.start <- 0
tortoise <- X.0
while(tortoise != hare){
  tortoise = f(tortoise)
  hare = f(hare)
  cycle.start <- cycle.start + 1
}

lam = 1
hare = f(tortoise)
while (tortoise != hare){
  hare = f(hare)
  lam <- lam + 1
}

print(lam)
