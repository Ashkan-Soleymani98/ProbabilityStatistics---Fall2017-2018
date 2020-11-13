library(ggplot2)
#1
times <- 20000
try <- c()
for(i in 2:5){
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
  title <- paste("Expected Value = ", sampleMean, "\nStandard Deviation = ", standardDeviation)
  print(ggplot(as.data.frame(means) , aes(x = as.data.frame(means))) + geom_density() + ggtitle(inf) + geom_vline(xintercept=sampleMean, colour="#BB0000", linetype="dashed", size=1) +
          geom_vline(xintercept=standardDeviation, colour="#008000", linetype="dashed", size=1) )
  print(paste0(i , " done")) 
}



