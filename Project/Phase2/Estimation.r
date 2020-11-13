p <- 0
b <- 0
a <- 0
lambda <- 0
k <- 0
teta <- 0
mu <- 0
sigma_2 <- 0
sample_vector <- vector("numeric")

sample_vector_creator <- function(string){
  sample_vector <<- scan(file = string, what = vector("numeric"))
}
uniformEstimation <- function(sample_vector){
  b <<- max(sample_vector)
  a <<- min(sample_vector)
  res <- c(a, b)
  res <- data.frame(res, row.names = c("minimum", "maximum"))
  return(res)
}
bernulliEstimation <- function(sample_vector){
  p <<- (sum(sample_vector)/length(sample_vector))
  res <- c(p)
  res <- data.frame(res, row.names = c("p"))
  return(res)
}
binomialEstimation <- function(sample_vector){
  p <<- (sum(sample_vector)/length(sample_vector))
  p <- (p / 100)
  res <- c(p)
  res <- data.frame(res, row.names = c("p"))
  return(res)
}
geometricEstimation <- function(sample_vector){
  p <<- (sum(sample_vector))/ (length(sample_vector))
  res <- c(p)
  res <- data.frame(res, row.names = c("p"))
  return(res)
}
exponentialEstimation <- function(sample_vector){
  lambda <<-length(sample_vector)/ (sum(sample_vector))
  res <- c(lambda)
  res <- data.frame(res, row.names = c("lambda"))
  return(res)
}
gammaEstimation <- function(sample_vector){
  first <- 0
  second <- 0
  n <- length(sample_vector)
  for(x in sample_vector){
    first <- first + (x * log(x))
    second <- second + log(x)
  }
  k <<- (length(sample_vector) * sum(sample_vector))/((n * first) - (sum(sample_vector) * second))
  teta <<- (length(sample_vector) * length(sample_vector))/(((n * first) - (sum(sample_vector) * second)))
  res <- c(k, teta)
  res <- data.frame(res, row.names = c("k", "teta"))
  return(res)
}
poissonEstimation <- function(sample_vector){
  lambda <<- (sum(sample_vector)/length(sample_vector))
  res <- c(lambda)
  res <- data.frame(res, row.names = c("lambda"))
  return(res)
}
normalEstimation <- function(sample_vector){
  sum <- 0
  mu <<- (sum(sample_vector)/length(sample_vector))
  for (x in sample_vector){
    sum <- sum + (x - mu)^2
  }
  sigma_2 <<- (sum/length(sample_vector))
  res <- c(mu, sum)
  res <- data.frame(res, row.names = c("expected value", "variance"))
  return(res)
}

