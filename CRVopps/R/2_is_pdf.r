library(cubature)

is_pdf <- function(){


  funcText <- readline(prompt = "Introduceti functia:")

  f <- function(x){}

  body(f) <- parse(text = funcText)


  rez <- cubintegrate(f, lower = -Inf, upper = Inf, method = "pcubature")

  ifelse( floor(rez$integral) == 1, return("Functia este densitate de probabilitate"), return ("Functia nu este densitate de probabilitate"))

}

is_pdf()

