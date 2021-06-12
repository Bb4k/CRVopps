library(cubature)

norm_const <- function(){


  my.func <- readline(prompt="Introduceti functia: ")


  resolve1 <-  function(x){

    eval(parse(text = paste('f <- function(', 'x', ') { return(' , my.func , ') }', sep='')))

    return (ifelse( f(x) == Inf | f(x) == -Inf, 0, f(x)))

  }



  rez  <-  cubintegrate(resolve1, lower = -Inf, upper = Inf, method = 'pcubature');


  ifelse ( is.na(rez$integral) | rez$integral == Inf | rez$integral == -Inf, "Functia introdusa nu are constanta de normalizare", 1/rez$integral )

}

norm_const()
