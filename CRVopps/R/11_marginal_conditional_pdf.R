library(cubature)
library(pracma)


marginal_density <- function(jpdf){

  low <- as.integer(readline(prompt="Define the sum domain:\nLow: \n"))
  high <- as.integer(readline(prompt="High: "))

  func <- function(x,y){}
  body(func) <- parse(text = jpdf)

  return(integral2(func, low, high, low, high)$Q)

}

conditional_density <- function(jpdf){

}




jpdf <- "exp(1)^(-((x^2)/2))"

marginals(jpdf)
