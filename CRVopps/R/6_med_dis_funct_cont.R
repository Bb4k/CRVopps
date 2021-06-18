#' Calculul mediei si dispersiei unei variabile aleatoare g(X),
#' unde X are o repartisie continua cunoscuta, iar g este o functie
#' continua precizata de utilizator
#'
#' @param f Densitatea de probabilitate a variabilei aleatoare continue.
#' @return Media \code{medie}, dispersia \code{dispersie}
#' @examples
#' medie_dispersie_func_cont(f)
medie_dispersie_func_cont <- function(f){

  calc_medie <- function(x) {
    x * f(x)
  }

  calc_patrat_medie <- function(x) {
    x*x*f(x)
  }

  out <- tryCatch(
    {
      medie <- integrate(f = Vectorize(calc_medie), lower = -Inf, upper = Inf)$value
      medie_patrat <- integrate(f = Vectorize(calc_patrat_medie), lower = -Inf, upper = Inf)$value
      dispersie <- medie_patrat - medie * medie
    },
    error=function(f) {
      message("Functia primita este divergenta, prin urmare nu putem calcula media!")
      return(NA)
    }
  )

  result <- list("Media variabilei aleatoare continue:" = medie, "Dispersia variabilei aleatoare continue:" = dispersie)

  if(is.na(out))
    return(out)
  return(result)

}

f <- function(x) {
  if(x >= 0 && x <= 1)
    (exp(1)*(exp(-x)+exp(x)))/(exp(2)-1)
  else 0
}


medie_dispersie_func_cont(f)






