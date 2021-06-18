create_crv <- function(f){


  type <- readline(prompt="1. Unidim\n2. Bidim\n")

  if(type == 1){

    x_low <- as.integer(readline(prompt="Low: "))
    x_high <- as.integer(readline(prompt="High: "))

    crv <- c(f, x_low, x_high)

    return(crv)

  }

  if(type == 2){

    x_low <- as.integer(readline(prompt="x Low: "))
    x_high <- as.integer(readline(prompt="x High: "))
    y_low <- as.integer(readline(prompt="y Low: "))
    y_high <- as.integer(readline(prompt="y High: "))


    crv <- c(f, x_low, x_high, y_low, y_high)

    return (crv)
  }
}

f <- function(x) {
  if(x >= 0 && x <= 1)
    (exp(1)*(exp(-x)+exp(x)))/(exp(2)-1)
  else 0
}

g <- function(x) {
  if((x >= 0) && (x <= pi)) {sin(x)/2} else {0}
}

h <- function(x) {
  if((x >= 0) && (x <= pi)) {
    sin(x)/2
  } else {0}
}

test <- create_crv(f)
print(test)
print(test[1])


