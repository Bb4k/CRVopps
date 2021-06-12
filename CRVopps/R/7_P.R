library(cubature)

P <- function(crv){

  sgn <- as.numeric(readline(prompt="Probabilitate:\n1. <\n2. >\n"))
  number <- as.numeric(readline(prompt = "Numar: "))

  func <- function(x){}

  body(func) <- parse(text = crv[1])

  val <- switch(
    sgn,
    cubintegrate(func, lower = as.integer(crv[2]),   upper = number, method = "pcubature"),
    cubintegrate(func, lower = number, upper = as.integer(crv[3]),    method = "pcubature")
  )

  return(val$integral)
}
