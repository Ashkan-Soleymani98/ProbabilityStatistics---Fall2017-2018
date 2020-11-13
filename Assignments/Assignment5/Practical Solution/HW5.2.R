#2
times <- 1000
try <- c()
errorsProb <- c()
for(i in 2:31){
  errors <- 0
  counter <- 0
  trys <- c()
  means <- c()
  while(counter < times){
    try <- runif(i , 0 , 1)
    trys <- c(trys , try)
    means <- c(means , mean(try))
    counter <- counter + 1
  }
  standardDeviation <- sd(means)
  sampleMean <- mean(means)
  for(j in 1:times){
    if(means[j] >= sampleMean + 0.005 || means[j] <= sampleMean - 0.005){
      errors <- errors + 1
    }
  }
  errorsProb <- c(errorsProb , as.double(errors/times))
}
print(errorsProb)
mat <- data.frame(c(2:31) , errorsProb)
library(gridExtra)
grid.table(mat)