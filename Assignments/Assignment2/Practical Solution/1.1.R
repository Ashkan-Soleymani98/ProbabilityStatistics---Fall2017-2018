m <- as.integer(readline(prompt="Enter modulus: "))
a <- as.integer(readline(prompt="Enter multiplier: "))
b <- as.integer(readline(prompt="Enter increment: "))
X.0 <- as.integer(readline(prompt="Enter seed: "))
randomVariableSequnces <- c(X.0)
temp <- X.0
for(i in 1:100){
  temp = temp * a
  temp = temp + b
  temp = temp %% m
  randomVariableSequnces <- c(randomVariableSequnces , temp)
}
print(randomVariableSequnces)