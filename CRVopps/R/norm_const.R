package(cubature)


constantaDeNormalizare <- function(){

  {
  my.func <- readline(prompt="Introduceti functia: ")


  resolve1 <-  function(x){

    eval(parse(text = paste('f <- function(', 'x', ') { return(' , my.func , ')}', sep='')))

    #return (ifelse( ((exp(1))^(( -(x^2)/2 ))) == Inf | ((exp(1))^(( -(x^2)/2 ))) == -Inf, 0, ((exp(1))^(( -(x^2)/2 ))) ))

    return (ifelse( f(x) == Inf | f(x) == -Inf, 0, f(x)))

  }



  rez  <-  cubintegrate(resolve1, lower = -Inf, upper = Inf, method = 'pcubature');
  }

  #integr <- function(x) { ((exp(1))^(( -(x^2)/2 )))*(1/rez$integral)}

  #rez2 = cubintegrate(integr, lower = -Inf, upper = Inf, method = 'pcubature');
  #eval(rez2$integral)

  ifelse ( is.na(rez$integral) | rez$integral == Inf | rez$integral == -Inf, "Functia introdusa nu are constanta de normalizare", 1/rez$integral )

}

constantaDeNormalizare()
