library(cubature)
package(cubature)

densitateDeProbabilitate <- function(){


  my.func <- readline(prompt="Introduceti functia: ")

  resolve1 <-  function(x){

    eval(parse(text = paste('f <- function(', 'x', ') { return(' , my.func , ')}', sep='')))

    #return (ifelse( ((exp(1))^(( -(x^2)/2 ))) == Inf | ((exp(1))^(( -(x^2)/2 ))) == -Inf, 0, ((exp(1))^(( -(x^2)/2 ))) ))

    return (ifelse( f(x) == Inf | f(x) == -Inf, 0, 0.3989423*f(x)))

  }


  # exp(1)^(-((x^2)/2))
  # rel.tol = 1e-15 eroare relativa
  rez <- cubintegrate(resolve1, lower = -Inf, upper = Inf, method = "pcubature")
  rez2 <- integrate(resolve1, lower = -Inf, upper = Inf)


  ifelse( floor(rez$integral) == 1, return("Functia este densitate de probabilitate"), return ("Functia nu este densitate de probabilitate"))

}



densitateDeProbabilitate()
