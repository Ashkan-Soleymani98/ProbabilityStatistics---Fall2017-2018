data <- read.csv("G:\\subjects\\statistics and probabilities\\HW6\\general math total groups score.csv")
data[data == "?"] <- 0
#view(data)

library(ggplot2)

#1
data$Department <- as.factor(data$Department)
print(ggplot(data, aes( x = Department, y = Final)) + geom_boxplot())

#2
boysScoresData <- data[data$Gender == "M" ,]
#View(boysScoresData)
girlsScoresData <- data[data$Gender == "F" ,]
#view(girlsScoresData)

boysScores <- ((boysScoresData$M1 + boysScoresData$M2 + boysScoresData$M3 + boysScoresData$Final)/4)
girlsScores <- ((girlsScoresData$M1 + girlsScoresData$M2 + girlsScoresData$M3 + girlsScoresData$Final)/4)

#print(boysScores)
#print(girlsScores)
par(mfrow=c(1,2))
hist(boysScores , main = "Boys")
hist(girlsScores , main = "Girls")

#3
dep1 <- data[data$Department == 20,]
dep2 <- data[data$Department == 21,]
dep3 <- data[data$Department == 22,]
dep4 <- data[data$Department == 24,]
dep5 <- data[data$Department == 25,]
dep6 <- data[data$Department == 26,]
dep7 <- data[data$Department == 27,]
dep8 <- data[data$Department == 28,]
dep9 <- data[data$Department == 40,]
dep10 <- data[data$Department == 45,]
dep11 <- data[data$Department == 49,]

dep1 <- data.frame((dep1$M1 + dep1$M2 + dep1$M3 + dep1$Final) /4)
dep2 <-  data.frame((dep2$M1 + dep2$M2 + dep2$M3 + dep2$Final) /4)
dep3 <-  data.frame((dep3$M1 + dep3$M2 + dep3$M3 + dep3$Final) /4)
dep4 <- data.frame((dep4$M1 + dep4$M2 + dep4$M3 + dep4$Final) /4)
dep5 <-  data.frame((dep5 $M1 + dep5 $M2 + dep5 $M3 + dep5 $Final) /4)
dep6 <-  data.frame((dep6$M1 + dep6$M2 + dep6$M3 + dep6$Final) /4)
dep7 <-  data.frame((dep7$M1 + dep7$M2 + dep7$M3 + dep7$Final) /4)
dep8 <-  data.frame((dep8$M1 + dep8$M2 + dep8$M3 + dep8$Final) /4)
dep9 <-  data.frame((dep9$M1 + dep9$M2 + dep9$M3 + dep9$Final) /4)
dep10 <-  data.frame((dep10$M1 + dep10$M2 + dep10$M3 + dep10$Final) /4)
dep11 <-  data.frame((dep11$M1 + dep11$M2 + dep11$M3 + dep11$Final) /4)


print(ggplot() + geom_density(data = dep1, aes(x=dep1),colour = "green")
      + geom_density(data = dep2, aes(x=dep2), colour = "red") 
      + geom_density(data = dep3, aes(x=dep3), colour = "blue") 
      + geom_density(data = dep4, aes(x=dep4), colour="yellow") 
      + geom_density(data = dep5, aes(x=dep5), colour= "orange") 
      + geom_density(data = dep6, aes(x=dep6), colour = "brown")
      + geom_density(data = dep7, aes(x=dep7), colour = "purple")
      + geom_density(data = dep8, aes(x=dep8),colour = "pink") 
      + geom_density(data = dep9, aes(x=dep9), colour = "gray") 
      + geom_density(data = dep10, aes(x=dep10),colour = "greenyellow") 
      + geom_density(data = dep11, aes(x=dep11))) 


