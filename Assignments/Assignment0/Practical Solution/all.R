#in all cases supposed power as srength

#part-1

#---start
my.dataframe <- read.csv("FullData.csv")
#---end


#part-2

#---start
col.num <- ncol(my.dataframe)
row.num <- nrow(my.dataframe)
print( paste("Dimention Of My Dataframe = (" , row.num ,"," ,col.num , ")") )
print("Names of the columns of my.dataframe :")
print(names(my.dataframe))
cat("\n\n")
#---end


#part-3

#---start
my.dataframe <- my.dataframe[order(my.dataframe["Height"]),]
View(my.dataframe)
#---end


#part-4

#---start
#-----explanation:First finding the uniques in club_postions as a vector then combining them to the National ones and finally find the uniques in total
positons_club <- c(unique((my.dataframe["Club_Position"])))
positons_national <- c(unique((my.dataframe["National_Position"])))
p <- c(unique(c(positons_national , positons_club)))
print("Different Types of Positions : ")
print(p)
cat("\n\n")
#---end


#part-5

#---start
#----explanation:First finding the Italians then printing the name
print("Italian Guys:")
print(my.dataframe[my.dataframe["Nationality"] == "Italy" , "Name"])
cat("\n\n")
#---end


#part-6

#---start
print("Columns`s mean:")
print(colMeans(my.dataframe[,sapply(my.dataframe , is.numeric)]))
cat("\n\n")
#---end


#part-7

#---start
#----explanation:Repeated way
print("70 <= Strength <= 90:")
print(my.dataframe[intersect(my.dataframe[,"Strength"] >= 70 , my.dataframe[,"Strength"] <= 90) , "Name"])
cat("\n\n")
#---end


#part-8

#---start
top.ten.dribler.finder <- function(dataframe){
  output <- dataframe[tail(order(dataframe$Dribbling), n = 100), ]
}
print("Top Ten Dribler:")
print(top.ten.dribler.finder(my.dataframe))
cat("\n\n")
#---end


#part9

#---start
spanish.players <- my.dataframe[my.dataframe["Nationality"] == "Spain",]
numOfthePlayers <- nrow(spanish.players)
mean.power <- mean(spanish.players[,"Strength"])
positons_club <- c(unique((spanish.players["Club_Position"])))
positons_national <- c(unique((spanish.players["National_Position"])))
postions <- c(unique(c(positons_national , positons_club)))
birthday <- c(spanish.players[,"Birth_Date"])
spanish.list <- list(spanish.players , numOfthePlayers , mean.power , postions , birthday)


#part10

#---start
print("Power o f Nations")
mean.power.natinality <- function(){
  tapply(my.dataframe$Strength , my.dataframe$Nationality , mean)
}
print(mean.power.natinality())
cat("\n\n")
#---end

#part11

#---start
loyal.finder <- function(){
  designateds <- vector()
  for(i in 1:nrow(my.dataframe)){
    honor <- my.dataframe[i, "Contract_Expiry"] - as.numeric(substr(toString(my.dataframe[i, "Club_Joining"]) , 7 , 10))
    designateds[i] <- honor
  }
  head(my.dataframe[order(designateds, decreasing = T) , "Name"] , 10)
}
print(loyal.finder())
#---end









