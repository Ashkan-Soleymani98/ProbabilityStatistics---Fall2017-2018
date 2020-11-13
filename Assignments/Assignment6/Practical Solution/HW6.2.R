data <- read.csv("G:\\subjects\\statistics and probabilities\\HW6\\general math total groups score.csv")
data[data == "?"] <- 0
#view(data)

BGradesData <- data[data$Teacher == "B",]
MGradesData <- data[data$Teacher == "M",]
SGradesData <- data[data$Teacher == "S",]
BGrades <- ((BGradesData$M1 + BGradesData$M2 + BGradesData$M3 + BGradesData$Final)/4)
MGrades <- ((MGradesData$M1 + MGradesData$M2 + MGradesData$M3 + MGradesData$Final)/4)
SGrades <- ((SGradesData$M1 + SGradesData$M2 + SGradesData$M3 + SGradesData$Final)/4)

conflevel <- 0.05

test <- t.test(BGrades, MGrades)
if(test$p.value < 0.05){
  print("With this confidence level there is a significant difference between Dr.B & Dr.M students' scores")
}else{
  print("With this confidence level we can't reject existense of a significant difference between Dr.B & Dr.M students' scores")
}
print(paste0("P value = " , test$p.value , " ,confidence is " , conflevel))

test <- t.test(BGrades , SGrades)
if(test$p.value < 0.05){
  print("With this confidence level there is a significant difference between Dr.B & Dr.S students' scores")
}else{
  print("With this confidence level we can't reject existense of a significant difference between Dr.B & Dr.S students' scores")
}
print(paste0("P value = " , test$p.value , " ,confidence is " , conflevel))

test <- t.test(MGrades , SGrades)
if(test$p.value < 0.05){
  print("With this confidence level there is a significant difference between Dr.M & Dr.S students' scores")
}else{
  print("With this confidence level we can't reject existense of a significant difference between Dr.M & Dr.S students' scores")
}
print(paste0("P value = " , test$p.value , " ,confidence is " , conflevel))


