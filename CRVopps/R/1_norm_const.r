library(cubature)

norm_const <- function(func){


  rez  <-  cubintegrate(func, lower = -Inf, upper = Inf, method = 'pcubature');


  ifelse ( is.na(rez$integral) | rez$integral == Inf | rez$integral == -Inf, "Functia introdusa nu are constanta de normalizare", round(1/rez$integral) )

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

norm_const(f)
