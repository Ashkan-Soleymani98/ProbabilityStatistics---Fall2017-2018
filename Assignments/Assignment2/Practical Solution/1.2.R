m <- as.integer(readline(prompt="Enter modulus: "))
a <- as.integer(readline(prompt="Enter multiplier: "))
b <- as.integer(readline(prompt="Enter increment: "))
X.0 <- as.integer(readline(prompt="Enter seed: "))

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
