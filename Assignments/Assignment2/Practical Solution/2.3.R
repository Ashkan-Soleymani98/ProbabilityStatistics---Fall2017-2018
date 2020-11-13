seed <- 100

f <- function(input){
  figure.num <- 0
  temp <- input
  while(temp > 0){
    temp <- floor(temp / 10)
    figure.num <- figure.num + 1
  }
  return(figure.num)
}

g <- function(input){
  sqaure <- input * input
  n <- f(input)
  if(f(sqaure) == 2 * n){
    if(n %% 2 == 0){
      sqaure <- sqaure %% (10 ^ (f(sqaure) - n / 2))
      sqaure <- floor(sqaure / (10 ^ (n / 2)))
    }else{
      sqaure <- sqaure %% (10 ^ (f(sqaure) - floor(n / 2)))
      sqaure <- floor(sqaure / (10 ^ (ceiling(n / 2))))
    }
  }else{
    n <- n - 1
    if(n %% 2 == 0){
      sqaure <- sqaure %% (10 ^ (f(sqaure) - n / 2))
      sqaure <- floor(sqaure / (10 ^ (n / 2)))
    }else{
      sqaure <- sqaure %% (10 ^ (f(sqaure) - floor(n / 2)))
      sqaure <- floor(sqaure / (10 ^ (ceiling(n / 2))))
    }
  }
  return(sqaure)
}

output <- c(seed)
temp <- seed
for(i in 1:1000){
  temp <- g(temp)
  output <- c(output , temp) 
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