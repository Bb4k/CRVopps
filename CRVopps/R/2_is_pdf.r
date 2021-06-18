library(cubature)

is_pdf <- function(func){


  rez <- cubintegrate(func, lower = -Inf, upper = Inf, method = "pcubature")

  ifelse( round(rez$integral) == 1, return("Functia este densitate de probabilitate"), return ("Functia nu este densitate de probabilitate"))

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

div <- function(x){
  x+1
}


is_pdf(div)

