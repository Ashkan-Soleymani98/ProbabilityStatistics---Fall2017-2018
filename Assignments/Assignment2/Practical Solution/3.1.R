seed <- as.integer(readline(prompt="Enter seed: "))

set.seed(seed)
print(sample(1 : 100) , size = 100 , replace=TRUE)