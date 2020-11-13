#please change the address before excution! Thank You:)
data <- read.csv("G:/subjects/statistics and probabilities/HW4/movie_metadata.csv")

correctData <- function(data){
  data <- na.omit(data)
  color <- vector("numeric")
  # assign 1 for colory and assign 0 for black and white to make the comparison possible!:)
  for(i in 1 : nrow(data)){
    if(data[i , "color"] == "Color"){
      color <- c(color , 1)
    }
    else{
      color <- c(color , 0)
    }
  }
  data <- data.frame(color , data$budget, data$num_critic_for_reviews,data$duration,data$director_facebook_likes,
                     data$actor_1_facebook_likes, data$num_voted_users, data$cast_total_facebook_likes,
                     data$num_user_for_reviews,data$imdb_score,data$movie_facebook_likes)
  return(data)
}

data <- correctData(data)

#part-1
covFinder <- function(data){
  covMatrix <- matrix(c(0), nrow = 11, ncol = 11)
  for(i in 1:11){
    for(j in 1:11){
      covMatrix[i,j] = cov(data[,i],data[,j])
    }
  }
  colnames(covMatrix) <- c("color" , "budget", "num critic for reviews", "duration", "director facebook likes", "actor 1 facebook likes", "num voted users", "cast total facebook likes", "num user for reviews","imdb score", "movie facebook likes")
  rownames(covMatrix) <- c("color" , "budget", "num critic for reviews", "duration", "director facebook likes", "actor 1 facebook likes", "num voted users", "cast total facebook likes", "num user for reviews","imdb score", "movie facebook likes")
  
  write.csv(covMatrix, "G:/subjects/statistics and probabilities/HW4/cov_mat.csv")
  
  return(covMatrix)
}

covMatrix <- covFinder(data)
View(covMatrix)

#part-2

#It can be seen that some features have a the same or opposite mean monotoy with film's success but in this way that if they have 
# positive/negetive Cov then they have the same/oppsite monotony but because it involves the mean amount , makes it hard to compare 
# effects of the fatures in success rate so to standardize this comparison (adjusting the scales) and easily comparing factors it's better to use correlations
# which is done below:

corFinder <- function(data){
  corMatrix <- matrix(c(0), nrow = 11, ncol = 11)
  for(i in 1:11){
    for(j in 1:11){
      corMatrix[i,j] = cor(data[,i],data[,j])
    }
  }
  colnames(corMatrix) <- c("color" , "budget", "num critic for reviews", "duration", "director facebook likes", "actor 1 facebook likes", "num voted users", "cast total facebook likes", "num user for reviews","imdb score", "movie facebook likes")
  rownames(corMatrix) <- c("color" , "budget", "num critic for reviews", "duration", "director facebook likes", "actor 1 facebook likes", "num voted users", "cast total facebook likes", "num user for reviews","imdb score", "movie facebook likes")
  
  write.csv(corMatrix, "G:/subjects/statistics and probabilities/HW4/cor_mat.csv")
  
  return(corMatrix)
}

corMatrix <- corFinder(data)
View(corMatrix)

#as it can be seen in the corMatrix : if we declare success with imdb score then these factors are the most improtant respectively as
# can be seen in the corMatrix:
#num voted users , duration , num critic for reviews , num user for reviews , movie facebook likes , director facebook likes 
# cast total facebook likes , actor 1 facebook likes , budget , color 
# it's clear that actor 1 & budget don;t have so effects!


#part-3
names <- c("color" , "budget", "num_critic_for_reviews", "duration", "director_facebook_likes", "actor_1_facebook_likes", "num _voted_users", "cast_total_facebook_likes", "num_user_for_reviews","imdb_score", "movie_facebook_likes")
for(i in 1:10){
  for(j in (i+1):11){
    plot(covMatrix[,i], covMatrix[,j],xlab = names[i], ylab = names[j])
  }
}
names <- c("color" , "budget", "num_critic_for_reviews", "duration", "director_facebook_likes", "actor_1_facebook_likes", "num _voted_users", "cast_total_facebook_likes", "num_user_for_reviews","imdb_score", "movie_facebook_likes")
for(i in 1:10){
  for(j in (i+1):11){
    plot(corMatrix[,i], corMatrix[,j],xlab = names[i], ylab = names[j])
  }
}


