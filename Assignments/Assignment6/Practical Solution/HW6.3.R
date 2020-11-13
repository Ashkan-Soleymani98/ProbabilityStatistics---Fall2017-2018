data <- read.csv("G:\\subjects\\statistics and probabilities\\HW6\\general math total groups score.csv")
data[data == "?"] <- 0
#view(data)

#according to the point that we want to find the maximum likelihood estimator and our distribution due to Centeral Limit Theorem is normal
#so it's enough to calculate sample mean of the midterms as prediction for the final score

avgs <- ((data$M1 + data$M2 + data$M3)/3)
finals <- (data$Final)

test <- t.test(finals , avgs , alternative = "less")
conflevel <- 0.05
if(test$p.value < 0.05){
  print("With this confidence level there is a significant overall progress between midterms &  final scores")
}else{
  print("With this confidence level we can't reject existense of a significant overall regress between midterms &  final scores")
}
print(paste0("P value = " , test$p.value , " ,confidence is " , conflevel))

