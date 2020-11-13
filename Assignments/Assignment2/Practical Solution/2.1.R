seed <- as.integer(readline(prompt="Enter seed: "))

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
for(i in 1:100){
  temp <- g(temp)
  output <- c(output , temp) 
}
print(output)
