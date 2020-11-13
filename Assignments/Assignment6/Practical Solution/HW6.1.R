data <- read.csv("G:\\subjects\\statistics and probabilities\\HW6\\general math total groups score.csv")
data[data == "?"] <- 0
#view(data)
boysScoresData <- data[data$Gender == "M" ,]
#View(boysScoresData)
girlsScoresData <- data[data$Gender == "F" ,]
#view(girlsScoresData)

boysScores <- ((boysScoresData$M1 + boysScoresData$M2 + boysScoresData$M3 + boysScoresData$Final)/4)
girlsScores <- ((girlsScoresData$M1 + girlsScoresData$M2 + girlsScoresData$M3 + girlsScoresData$Final)/4)

#print(boysScores)
#print(girlsScores)

conflevel <- 0.05
test <- t.test(boysScores , girlsScores)
if(test$p.value < 0.05){
  print("With this confidence level there is a significant difference between girls & boys scores")
}else{
  print("With this confidence level we can't reject existense of a significant difference between girls & boys scores")
}
print(paste0("P value = " , test$p.value , " ,confidence is " , conflevel))