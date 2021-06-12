library(cubature)

norm_const <- function(){

  funcText <- readline(prompt = "Introduceti functia:")

  f <- function(x){}

  body(f) <- parse(text = funcText)

  a <- as.integer(readline(prompt="Low: "))
  b <- as.integer(readline(prompt="Hi: "))

  if(is.na(a))
    a <- -Inf
  if(is.na(b))
    b <- Inf


  rez  <-  cubintegrate(f, lower = a, upper = b, method = 'pcubature');


  ifelse ( is.na(rez$integral) | rez$integral == Inf | rez$integral == -Inf, "Functia introdusa nu are constanta de normalizare", 1/rez$integral )

}

norm_const()
