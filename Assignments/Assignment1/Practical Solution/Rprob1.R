#---problem1
numOfEdges <- c()
numOfGraphs <- c()
h <- 20
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

framework1 <- data.frame(numOfEdges , numOfGraphs)
View(framework1)