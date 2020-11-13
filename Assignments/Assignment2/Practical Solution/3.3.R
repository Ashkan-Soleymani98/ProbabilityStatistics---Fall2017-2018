seed <- 100

f <- function(temp){
  a <- sample(1 : 100 , size = 1 , replace=TRUE)
}

output <- c()
for(i in 1 : 1000){
  output <- c(output , f(0))
}

print(output)

#---
tortoise = seed
hare = f(seed)
while(tortoise != hare){
  tortoise = f(tortoise)
  hare = f(f(hare))
}
#---
cycle.start <- 0
tortoise <- seed
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