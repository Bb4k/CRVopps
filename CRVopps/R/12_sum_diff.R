library(pracma)

sum <- function(crv1, crv2){

  low <- as.integer(readline(prompt="Define the sum domain:\nLow: \n"))
  high <- as.integer(readline(prompt="High: "))

  new_func <- function(z, y){}
  crv1_func_retext <- gsub("x", "(z-y)", crv1[1])
  crv2_func_retext <- gsub("x", "y", crv2[1])

  new_func_body <-  paste(crv1_func_retext, crv2_func_retext, sep="*")

  body(new_func) <- parse(text = new_func_body)

  return(integral2(new_func, low, high, low, high))

}

crv1 <- c("x+1", 0, 10)
crv2 <- c("x-1", 0, 10)

sum(crv1, crv2)
