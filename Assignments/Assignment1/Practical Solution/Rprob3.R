#---problem1
numOfEdges <- c()
numOfGraphs <- c()
h <- 20 #numOfNodes
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

#---problem2and3 ----> all explained in Latex file
numOfConnectedGraphs <- c()
matrixs <- list()

for(i in 1 : 190){
  numOfConnectedGraphs[i] <- 0
  c <- 0
  m <- 0
  numOfEval <- 10
  for(q in 1 : numOfEval){
    m <- 0
    a <- list()
    b <- 0
    while(b < (numOfGraphs[i] / 4)){
      #print(b)
      flag <- 0
      matrix1 <- matrix(rep(0, len = h * h) , ncol = h , nrow = h)
      j <- 0
      while (j < i) {
        x <- ceiling(runif(1 , 0 , h))
        y <- ceiling(runif(1 , 0 , h))
        if(matrix1[x , y] != 1){
          if(x != y){
            matrix1[x , y] <- 1
            j <- j + 1
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
          m <- m + 1
        }
      }
    }
    c <- (4 * m) + c
    #print(c)
  }
  numOfConnectedGraphs[i] = floor(c / numOfEval)
  print(numOfConnectedGraphs[i])
}

framework2 <- data.frame(numOfEdges , numOfGraphs , numOfConnectedGraphs)
View(framework2)
