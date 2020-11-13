#---problem1
numOfEdges <- c()
numOfGraphs <- c()
h <- 5
comb <- function(n){
  a <- 1
  if(n == (h * (h - 1) / 2))
    return(1)
  if(n > ((h * (h - 1) / 2)/2)){
    for(j in (n + 1) : (h * (h - 1) / 2)){
      a <- a * j
    }
    for(j in 1 : ((h * (h - 1) / 2) - n)){
      a <- a / j
    }
  }else{
    for(j in ((h * (h - 1) / 2) - n + 1) : (h * (h - 1) / 2)){
      a <- a * j
    }
    for(j in 1 : n){
      a <- a / j
    }
  }
  return(a)
}

for(i in 1 : (h * (h - 1) / 2)){
  numOfEdges[i] = i
  numOfGraphs[i] = comb(i)
}

#framework1 <- data.frame(numOfEdges , numOfGraphs)
#View(framework1)
 
#---problem2and3 ----> algorithm is right (explained in Latex file) but I had problems only with syntax in these parts(2 , 3)
numOfConnectedGraphs <- c()
matrixs <- list()

for(i in 1 : 10){
  numOfConnectedGraphs[i] <- 0
  a <- list()
  b <- 0
  if(i < h - 1){
    next
  }else if(i > ((h - 1) * (h - 2) / 2)){
    numOfConnectedGraphs[i] <- numOfGraphs[i]
    next
  }
  while(b < numOfGraphs[i]){
    #print(b)
    flag <- 0
    matrix1 <- matrix(rep(0, len = h * h) , ncol = h , nrow = h)
    j <- 0
    while (j < i) {
      x <- ceiling(runif(1 , 0 , h))
      y <- ceiling(runif(1 , 0 , h))
      if(matrix1[x , y] != 1){
        if(matrix1[y , x] != 1){
          if(x != y){
            matrix1[x , y] <- 1
            matrix1[y , x] <- 1
            j <- j + 1
          }
        }
      } 
    }
    #print(matrix1)
    if(length(a) > 0){
      for(k in 1:length(a)){
        if(all(matrix1 == a[[k]])){
          flag <- 1
          break
        }
      }
    }
    if(flag == 1){
      flag = 0
    }else{
      a[[(length(a) + 1)]] <- matrix1
      b <- b + 1
      matrix2 <- matrix1
      for(w in 1:(h - 1)){
        matrix2 <- matrix2 + (matrix2 %*% matrix1)
      }
      flag2 <- 0
      for(w in 1:h){
        for(r in 1:h){
          if(w != r && matrix2[w , r] == 0){
            flag2 <- 1
            break
          }
        }
      }
      if(flag2 != 1){
        numOfConnectedGraphs[i] <- numOfConnectedGraphs[i] + 1
      }
    }
  }
}

framework2 <- data.frame(numOfEdges , numOfGraphs , numOfConnectedGraphs)
View(framework2)
