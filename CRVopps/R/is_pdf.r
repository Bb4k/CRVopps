library(cubature)
package(cubature)

densitateDeProbabilitate <- function(){

  {
  my.func <- readline(prompt="Introduceti functia: ")

  eval(parse(text = paste('f <- function(', 'x', ') { return(' , my.func , ')}', sep='')))
  }


  resolve1 <-  function(x){

    eval(parse(text = paste('f <- function(', 'x', ') { return(' , my.func , ')}', sep='')))


    return (ifelse( f(x) == Inf | f(x) == -Inf, 0, 0.3989423*f(x)))

  }


  rez <- cubintegrate(resolve1, lower = -Inf, upper = Inf, method = "pcubature")



    ifelse( rez$integral == 1, return("Functia este densitate de probabilitate"), return ("Functia nu este densitate de probabilitate"))

}

densitateDeProbabilitate()
