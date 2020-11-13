#part-1
library(ggplot2)
data = data.frame(rexp(n = 1000 , 0.1))
print(ggplot(data, aes(x = data)) + geom_density())

#part-2 --sal e kabise! by default starting with kabise!
myFunc <- function(n){
  intervals <- vector("numeric")
  calculated_years <- 0
  years <- vector("numeric")
  num_each_year <- ("numeric")
  while(calculated_years < n){
    counter <- 0
    if(calculated_years %% 4 != 0){
      while(sum(intervals) < 366){
        intervals <- c(intervals , rexp(n = 1 , 0.1))
        counter <- counter + 1;
      }
    }else{
      while (sum(intervals) < 365) {
        intervals <- c(intervals , rexp(n = 1 , 0.1))
        counter <- counter + 1;
      }
    }
    years <- c(years , (calculated_years + 1))
    num_each_year <- c(num_each_year , counter)
    calculated_years <- calculated_years + 1
    intervals <- vector("numeric")
  }
  #print(num_each_year)
  return (num_each_year)
}

#print(myFunc(10))

#part-3 
numberInEachYear <- as.data.frame(myFunc(10))
ggplot(numberInEachYear, aes(x = numberInEachYear)) + geom_density()

numberInEachYear <- as.data.frame(myFunc(100))
ggplot(numberInEachYear, aes(x = numberInEachYear)) + geom_density()

numberInEachYear <- as.data.frame(myFunc(1000))
ggplot(numberInEachYear, aes(x = numberInEachYear)) + geom_density()

#part-4
myFunc2 <- function(n){
  nth_v <- vector("numeric")
  time <- vector("numeric")
  while(n > 0){
    n <- n-1
    days <- c(rexp(10, 0.1))
    time <- c(time, round(sum(days)))
  }
  print(time)
  return(time)
}

#part-5
times <- as.data.frame(myFunc2(1000))
ggplot(times, aes(x = times)) + geom_density()





