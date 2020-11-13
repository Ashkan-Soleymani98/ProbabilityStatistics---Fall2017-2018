
recent <- 29


rgen <- function(num)
{
  seed <- recent
  m <- ((2**31) - 1)
  a <- 1103515245
  c <- 12345
  rand <- c(seed)
  for(i in 1:(num+1))
  {
    temp <- a * seed
    temp <- temp + c
    temp <- temp %% m
    rand <- c(rand, temp)
    seed <- temp
  }
  for(i in 2:(num+1))
  {
    rand[i] <- (rand[i] / (2**31))
  }
  seed <- (seed + as.integer((as.double(Sys.time())*1000+Sys.getpid()) %% 50))
  assign("recent", seed, envir = .GlobalEnv)
  return(rand[2:(num+1)])
}





dugen <- function(st, en, num)
{
  rands <- rgen(num)
  for(i in 1:num)
  {
    rands[i] <-  ((rands[i]* (en - st)) + st)
  }
  return(rands)
}


cugen <- function(num)
{
  rands <- rgen(num)
  return(rands)
}

brgen <- function(p, num)
{
    
    rands <- cugen(num)
    for(i in 1:num)
    {
      if(rands[i] > p)
      {
        rands[i] <- 0
      }
      else
      {
        rands[i] <- 1
      }
    }
  
  return(rands)
  
}

bigen <- function(p, num)
{

  rands <- brgen(p, num)
  
  result <- 0
  for(i in 1:num)
  {
    if(rands[i] == 1)
    {
      result <- result + 1
    }
  }
  return(result)
}




gegen3 <- function(p)
{
  count <- 0
  while(TRUE)
  {
    tr <- brgen(p,1)
    if(tr == 0)
    {
      count <- (count + 1)
    }
    else
    { 
      break()
    }
  }
  
  return(count)
} 

library(ggplot2)



expgen <- function(lambda){
  x<-(cugen(1)[1])
  result<-((-1/lambda)*log(x))
  return(result)
}
gagen <- function(lambda, k){
  result <- 0
  for (i in 1:k){
    result <- result + expgen(lambda)
  }
  return(result)
}
pogen <- function(lambda, t){
  times <- vector()
  counter <- 0
  while (sum(times) < t){
    times <- c(times, expgen(lambda))
    counter <- counter + 1
  }
  return(counter)
}

nogen<- function(u,s){
  lambda <- 10
  tLength <- 10
  zResult=(pogen(lambda,tLength)-(lambda*tLength))/sqrt(lambda)
  result=zResult*(sqrt(s)) + u
  return(result)
}

library(ggplot2)
visualizeCu <- function(){
  
  test <- vector()
  for (i in 1:10000){
    test <- c(test, cugen(1))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
  
}

visualizeDu <- function(st,en){
  
  test <- vector()
  for (i in 1:10000){
    test <- c(test, dugen(st,en,1))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
  
}

visualizebr <- function(p){
  
  test <- vector()
  for (i in 1:10000){
    test <- c(test, brgen(p,1))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
  
}

visualizebi <- function(p,num){
  
  test <- vector()
  for (i in 1:1000){
    test <- c(test, bigen(p,num))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
  
}

visualizegeo <- function(p){
  
  test <- vector()
  for (i in 1:1000){
    test <- c(test, gegen3(p))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
  
}

visualizeEXP <- function(lambda){
  
  test <- vector()
  for (i in 1:10000){
    test <- c(test, expgen(lambda))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
}
visualizeGAMMA <- function(lambda, k){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, gagen(lambda, k))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
}
visualizePOISSON <- function(lambda, t){
test <- vector()
for (i in 1:1000){
  test <- c(test, pogen(lambda, t))
}
hist(test)
test <- as.data.frame(test)
ggplot(test, aes(x = test))+ geom_density()
}

visualizeNORMAL <- function(u, s){
  test <- vector()
  for (i in 1:1000){
    test <- c(test, nogen(u, s))
  }
  hist(test)
  test <- as.data.frame(test)
  ggplot(test, aes(x = test))+ geom_density()
  
}
