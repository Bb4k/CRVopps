#' Acesta functie efectueaza calculul mediei, dispersiei si a momentelor
#' initiale si centrate pana la ordinul 4(daca exista). Atunci cand unul dintre
#' momente nu exista, se va afisa un mesaj corespunzator catre utilizator
#'
#' @param f Densitatea de probabilitate a variabilei aleatoare continue.
#' @return Media \code{medie}, dispersia \code{dispersie},
#' lista de momente initiale \code{vector_mom_initial},
#' lista de momente centrate \code{vector_mom_centrate}.
#' @examples
#' medie_dipsersie_momente(f)

medie_dispersie_momente <- function(f){

  fct_medie <- function(x) {
    x * f(x)
  }

  fct_patrat_medie <- function(x) {
    x*x*f(x)
  }
  out <- tryCatch(
    {
      medie <- integrate(f = Vectorize(fct_medie), lower = -Inf, upper = Inf)$value

      medie_patrat <- integrate(f = Vectorize(fct_patrat_medie), lower = -Inf, upper = Inf)$value
      dispersie <- medie_patrat - medie * medie

    },
    error=function(f) {
      message("Functia primita este divergenta, prin urmare nu putem calcula media!")
      return(NA)
    }
  )

  calc_mom_initial <- function() {
    vector_mom_initial <- list()
    for (i in 1:4){
      tryCatch({
        fct_mom_initial <- function(x){
          (x-medie)^i*f(x)
        }
        res <- integrate(f = Vectorize(fct_mom_initial), lower = -Inf, upper = Inf)$value
        vector_mom_initial <- append(vector_mom_initial, res)
      },
      error=function(f) {
        message("Momentul initial de ordin ", i, " nu se poate determina, deoarece functia (x-E(x))^",i, "*f(x) este divergenta!")
        return(NA)
      })
    }
    return(vector_mom_initial)
  }


  calc_mom_centrat <- function(x) {
    vector_mom_centrate <- list()
    for (i in 1:4){
      tryCatch({
        fct_mom_centrat <- function(x){
          x^i*f(x)
        }
        res <- integrate(f = Vectorize(fct_mom_centrat), lower = -Inf, upper = Inf)$value
        vector_mom_centrate <- append(vector_mom_centrate, res)
      },
      error=function(f) {
        message("Momentul centrat de ordin ", i, " nu se poate determina, deoarece functia (x-E(x))^",i, "*f(x) este divergenta!")
        return(NA)
      })
    }
    return(vector_mom_centrate)
  }

  vector_mom_initial = calc_mom_initial()
  vector_mom_centrate = calc_mom_centrat()

  result <- list("Media variabilei aleatoare continue:" = medie,
                 "Dispersia variabilei aleatoare continue:" = dispersie,
                 "Momentele initiale ale variabilei aleatoare continue:" = vector_mom_initial,
                 "Momentele centrate ale variabilei aleatoare continue:" = vector_mom_centrate)

  if(is.na(out))
    return(out)
  return(result)

}

f <- function(x) {
  if (x >= 0 && x <= 1)
    (exp(1) * (exp(-x) + exp(x))) / (exp(2) - 1)
  else
    0
}

g <- function(x) {
  if ((x >= 0) && (x <= pi)) {
    sin(x) / 2
  } else {
    0
  }
}


f1 <- function(x) {
  x*x
}

medie_dispersie_momente(f)

