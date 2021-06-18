#' @name Densitatea Repartitiilor si Functiile de Repartitie
#'
#' Acesta functie realizeaza graficele densitatilor repartitiilor, respectiv
#' functiilor de repartitie uniforme, Bernoulli, binomiale (si negativ binomiale),
#' geometrice, hypergeometrice, exponentiale, Poisson, normale, log-normale.
#' Weibull, Student's T, gamma, beta, Chi-Square si Cauchy. In # total 16 tipuri.
#'
#' @param f Densitatea de probabilitate a variabilei alea# toare continue.
#' @return Media \code{medie}, dispersia \code{dispersie},
#' lista de momente initiale \code{vec# tor_mom_initial},
#' lista de momente centrate \code{vec# tor_mom_centrate}.
#' @examples
#' medie_dipsersie_momente(f)

library("statip")
require("statip")

graf_repartitii_dens <- function()
{
# Densitati de repartitie -------------------------------------------------

  dens_unif <- function(min, max){
    curve(expr = dunif(x = x, min = min, max = max),
          main = "The Uniform Distribution",
          xlab = "x",
          ylab = "f(x)",

           from= -2 * max,
             to = 2 * max,

          col = "red",
          lwd = 3
    )
  }

  dens_norm <- function(mean, sd){
    curve(expr = dnorm(x = x, mean = mean, sd = sd),
          main = "The Normal Distribution",
          xlab = "x",
          ylab = "f(x)",

          from = mean + (-2 * sd),
            to = mean + 2 * sd,

          col = "red",
          lwd = 3
    )
  }

  dens_exp <- function(rate){
    curve(expr = dexp(x = x, rate = rate),
          main = "The Exponential Distribution",
          xlab = "x",
          ylab = "f(x)",

          # from= -rate,
            # to = rate,

          col = "red",
          lwd = 3
    )
  }


  dens_cauchy <- function(location, scale){
    curve(expr = dcauchy(x = x, location = location, scale = scale),
          main = "The Cauchy Distribution",
          xlab = "x",
          ylab = "f(x)",

          # from= location + (-5 * rate),
            # to = location + 5 * rate,

          col = "red",
          lwd = 3
    )
  }

  dens_beta <- function(shape1, shape2){
    curve(expr = dbeta(x = x, shape1 = shape1, shape2 = shape2),
          main = "The Beta Distribution",
          xlab = "x",
          ylab = "f(x)",

           from= 0,
             to = 1,
          # limitele sunt atinse pt x={0,1}
          # abaterea =  shape1*shape2/((shape1 + shape2)^2*(shape1+shape2+1))
          # media = shape1/(shape1 + shape2)
          # dar nu am reusit sa creez o formula cu ele

          col = "red",
          lwd = 3
    )
  }

  dens_gamma <- function(shape, rate){
    curve(expr = dgamma(x = x, shape = shape, rate = rate),
          main = "The Gamma Distribution",
          xlab = "x",
          ylab = "f(x)",

           from= 0,
             to = rate * (shape<rate) + shape * (shape>=rate),

          col = "red",
          lwd = 3
    )
  }

  dens_chisq <- function(df, ncp){
    curve(expr = dchisq(x = x, df = df, ncp = ncp),
          main = "The (non-central) Chi-Squared Distribution",
          xlab = "x",
          ylab = "f(x)",

          from = 0,
            to = ncp + ncp*df,

          col = "red",
          lwd = 3
    )
  }

  dens_lnorm <- function(meanlog, sdlog){
    curve(expr = dlnorm(x = x, meanlog = meanlog, sdlog = sdlog),
          main = "The Log Normal Distribution",
          xlab = "x",
          ylab = "f(x)",

          from= meanlog-3*sdlog,
             to = meanlog+3*sdlog,

          # media - abaterea = exp(meanlog + 1/2*(sdlog^2)) + exp(2*meanlog + sdlog^2)*(exp(sdlog^2)-1)

          col = "red",
          lwd = 3
    )
  }

  dens_bern <- function(prob){
    curve(expr = dbern(x = x, prob = prob),
          main = "The Bernoulli Distribution",
          xlab = "x",
          ylab = "f(x)",

          # from= -prob,
            # to = prob,

          col = "red",
          lwd = 3
    )
  }

  dens_binom <- function(size, prob){
    curve(expr = dbinom(x = x, size = size, prob = prob),
          main = "The Binomial Distribution",
          xlab = "x",
          ylab = "f(x)",

          from= -size,
          to = size,

          col = "red",
          lwd = 3
    )
  }

  dens_nbinom <- function(size, prob){
    curve(expr = dnbinom(x = x, size = size, prob = prob),
          main = "The Negative Binomial Distribution",
          xlab = "x",
          ylab = "f(x)",

          from= -size,
          to = size,

          col = "red",
          lwd = 3
    )
  }

  dens_geom <- function(prob){
    curve(expr = dgeom(x = x, prob = prob),
          main = "The Geometric Distribution",
          xlab = "x",
          ylab = "f(x)",

          # from= -(prob^2),
          # to = prob^2,

          col = "red",
          lwd = 3
    )
  }

  dens_hyper <- function(m, n, k){
    curve(expr = dhyper(x = x, m = m, n = n, k = k),
          main = "The Hypergeometric Distribution",
          xlab = "x",
          ylab = "f(x)",

          # from= 0,
            # to = m+n,

          col = "red",
          lwd = 3
    )
  }

  dens_pois <- function(lambda){
    curve(expr = dpois(x = x, lambda = lambda),
          main = "The Poisson Distribution",
          xlab = "x",
          ylab = "f(x)",

           from= 0,
           to = 2*lambda,

          col = "red",
          lwd = 3
    )
  }

  dens_weibull <- function(shape, scale){
    curve(expr = dweibull(x = x, shape = shape, scale = scale),
          main = "The Weibull Distribution",
          xlab = "x",
          ylab = "f(x)",

           from= 0,
           to = 4,

          col = "red",
          lwd = 3
    )
  }

  dens_student <- function(df, ncp){
    curve(expr = dt(x = x, df = df, ncp = ncp),
          main = "The Student's T Distribution",
          xlab = "x",
          ylab = "f(x)",

           from= 0,
           to = ncp + ncp*df,

          col = "red",
          lwd = 3
    )
  }

  # -------------------------------------------------------------------------


  # Functii de Repartitii ---------------------------------------------------

  rep_unif <- function(min, max){
    curve(expr = punif(q = x, min = min, max = max),
          main = "The Uniform Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= -3 * max,
            # to = 3 * max,

          col = "red",
          lwd = 3
    )
  }

  rep_norm <- function(mean, sd){
    curve(expr = pnorm(q = x, mean = mean, sd = sd),
          main = "The Normal Distribution Function",
          xlab = "x",
          ylab = "F(x)",

           from= mean + (-2 * sd),
             to = mean + 2 * sd,

          col = "red",
          lwd = 3
    )
  }

  rep_exp <- function(rate){
    curve(expr = pexp(q = x, rate = rate),
          main = "The Gamma Distribution",
          xlab = "x",
          ylab = "F(x)",

          # from= 0,
          # to = rate * (shape<rate) + shape * (shape>=rate),

          col = "red",
          lwd = 3
    )
  }

  rep_cauchy <- function(location, scale){
    curve(expr = pcauchy(q = x, location = location, scale = scale),
          main = "The Cauchy Distribution Function",
          xlab = "x",
          ylab = "F(x)",

           from= location + (-5 * scale),
           to = location + 5 * scale,

          col = "red",
          lwd = 3
    )
  }

  rep_beta <- function(shape1, shape2){
    curve(expr = pbeta(q = x, shape1 = shape1, shape2 = shape2),
          main = "The Beta Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= 0,
          # to = 1,

          col = "red",
          lwd = 3
    )
  }

  rep_gamma <- function(shape, rate){
    curve(expr = pgamma(q = x, shape = shape, rate = rate),
          main = "The Gamma Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= 0,
          # to = rate * (shape<rate) + shape * (shape>=rate),

          col = "red",
          lwd = 3
    )
  }

  rep_chisq <- function(ncp){
    curve(expr = pchisq(q = x, ncp = ncp),
          main = "The (non-central) Chi-Squared Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= 0,
          # to = ncp + ncp*df,

          col = "red",
          lwd = 3
    )
  }

  rep_lnorm <- function(meanlog, sdlog){
    curve(expr = plnorm(q = x, meanlog = meanlog, sdlog = sdlog),
          main = "The Log Normal Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= exp(meanlog + sdlog) - exp(meanlog + sdlog)*(exp(sdlog)-1),
          # to = exp(meanlog + sdlog) + exp(meanlog + sdlog)*(exp(sdlog)-1),

          col = "red",
          lwd = 3
    )
  }

  rep_bern <- function(prob){
    curve(expr = pbern(q = x, prob = prob),
          main = "The Bernoulli Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= -prob*100,
            # to = prob*100,

          col = "red",
          lwd = 3
    )
  }

  rep_binom <- function(size, prob){
    curve(expr = pbinom(q = x, size = size, prob = prob),
          main = "The Binomial Distribution Function",
          xlab = "x",
          ylab = "F(x)",

           from= -prob*100,
           to = prob*100,

          col = "red",
          lwd = 3
    )
  }

  rep_nbinom <- function(size, prob){
    curve(expr = pnbinom(q = x, size = size, prob = prob),
          main = "The Negative Binomial Distribution Function",
          xlab = "x",
          ylab = "F(x)",

           from= -(prob^5),
           to = prob^2,

          col = "red",
          lwd = 3
    )
  }

  rep_geom <- function(prob){
    curve(expr = pgeom(q = x, prob = prob),
          main = "The Geometric Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= -(prob^2),
          # to = prob^2,

          col = "red",
          lwd = 3
    )
  }

  rep_hyper <- function(m, n, k){
    curve(expr = phyper(q = x, m = m, n = n, k = k),
          main = "The Hypergeometric Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= 0,
          # to = m+n,

          col = "red",
          lwd = 3
    )
  }

  rep_pois <- function(lambda){
    curve(expr = ppois(q = x, lambda = lambda),
          main = "The Poisson Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= -lambda,
          # to = lambda^2,

          col = "red",
          lwd = 3
    )
  }

  rep_weibull <- function(shape, scale){
    curve(expr = pweibull(q = x, shape = shape, scale = scale),
          main = "The Weibull Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= -shape*scale,
          # to = shape*scale,

          col = "red",
          lwd = 3
    )
  }

  rep_student <- function(df, ncp){
    curve(expr = pt(q = x, df = df, ncp = ncp),
          main = "The Student's T Distribution Function",
          xlab = "x",
          ylab = "F(x)",

          # from= 0,
          # to = ncp + ncp*df,

          col = "red",
          lwd = 3
    )
  }


  # -------------------------------------------------------------------------

  afisare_dens <- function() {
    cat("Meniu:\n")
    cat("1. Uniform\n")
    cat("2. Bernoulli\n")
    cat("3. Binomial\n")
    cat("4. Negative Binomial\n")
    cat("5. Geometric\n")
    cat("6. Hypergeometric\n")
    cat("7. Exponential\n")
    cat("8. Poisson\n")
    cat("9. Normal\n")
    cat("10. Weibull\n")
    cat("11. Log-Normal\n")
    cat("12. Student's T\n")
    cat("13. Gamma\n")
    cat("14. Beta\n")
    cat("15. Chi-Square\n")
    cat("16. Cauchy\n")

    option <- as.numeric(readline(prompt = "Optiune: "))

    # curata consola pentru a nu crea confuzie
    cat("\014")

    switch(  EXPR = option,
             {
               min = as.numeric(readline(prompt = "min: "))
               max = as.numeric(readline(prompt = "max: "))
               dens_unif(min, max)
             },
             {
               prob = as.numeric(readline(prompt = "prob: "))
               dens_bern(prob)
             },
             {
               size = as.numeric(readline(prompt = "size: "))
               prob = as.numeric(readline(prompt = "prob: "))
               dens_binom(size, prob)
             },
             {
               size = as.numeric(readline(prompt = "size: "))
               prob = as.numeric(readline(prompt = "prob: "))
               dens_nbinom(size, prob)
             },
             {
               prob = as.numeric(readline(prompt = "prob: "))
               dens_geom(prob)
             },
             {
               m = as.numeric(readline(prompt = "m: "))
               n = as.numeric(readline(prompt = "n: "))
               k = as.numeric(readline(prompt = "k: "))
               dens_hyper(m, n, k)
             },
             {
               rate = as.numeric(readline(prompt = "rate: "))
               dens_exp(rate)
             },
             {
               lambda = as.numeric(readline(prompt = "lambda: "))
               dens_pois(lambda)
             },
             {
               mean = as.numeric(readline(prompt = "mean: "))
               sd = as.numeric(readline(prompt = "sd: "))
               dens_norm(mean, sd)
             },
             {
               shape = as.numeric(readline(prompt = "shape: "))
               scale = as.numeric(readline(prompt = "scale: "))
               dens_weibull(shape, scale)
             },
             {
               meanlog = as.numeric(readline(prompt = "meanlog: "))
               sdlog = as.numeric(readline(prompt = "sdlog: "))
               dens_lnorm(meanlog = meanlog, sdlog)
             },
             {
               df = as.numeric(readline(prompt = "df: "))
               ncp = as.numeric(readline(prompt = "ncp: "))
               dens_student(df, ncp)
             },
             {
               shape = as.numeric(readline(prompt = "shape: "))
               rate = as.numeric(readline(prompt = "rate: "))
               dens_gamma(shape, rate)
             },
             {
               shape1 = as.numeric(readline(prompt = "shape1: "))
               shape2 = as.numeric(readline(prompt = "shape2: "))
               dens_beta(shape1, shape2)
             },
             {
               df = as.numeric(readline(prompt = "df: "))
               ncp = as.numeric(readline(prompt = "ncp: "))
               dens_chisq(df, ncp)
             },
             {
               location = as.numeric(readline(prompt = "location: "))
               scale = as.numeric(readline(prompt = "scale: "))
               dens_cauchy(location, scale)
             }
    )
  }

  afisare_rep <- function() {
    cat("Meniu:\n")
    cat("1. Uniform\n")
    cat("2. Bernoulli\n")
    cat("3. Binomial\n")
    cat("4. Negative Binomial\n")
    cat("5. Geometric\n")
    cat("6. Hypergeometric\n")
    cat("7. Exponential\n")
    cat("8. Poisson\n")
    cat("9. Normal\n")
    cat("10. Weibull\n")
    cat("11. Log-Normal\n")
    cat("12. Student's T\n")
    cat("13. Gamma\n")
    cat("14. Beta\n")
    cat("15. Chi-Square\n")
    cat("16. Cauchy\n")

    option <- as.numeric(readline(prompt = "Optiunea: "))

    # curata consola pentru a nu crea confuzie
    cat("\014")

    switch(  EXPR = option,
             {
               min = as.numeric(readline(prompt = "min: "))
               max = as.numeric(readline(prompt = "max: "))
               rep_unif(min, max)
             },
             {
               prob = as.numeric(readline(prompt = "prob: "))
               rep_bern(prob)
             },
             {
               size = as.numeric(readline(prompt = "size: "))
               prob = as.numeric(readline(prompt = "prob: "))
               rep_binom(size, prob)
             },
             {
               size = as.numeric(readline(prompt = "size: "))
               prob = as.numeric(readline(prompt = "prob: "))
               rep_nbinom(size, prob)
             },
             {
               prob = as.numeric(readline(prompt = "prob: "))
               rep_geom(prob)
             },
             {
               m = as.numeric(readline(prompt = "m: "))
               n = as.numeric(readline(prompt = "n: "))
               k = as.numeric(readline(prompt = "k: "))
               rep_hyper(m, n, k)
             },
             {
               rate = as.numeric(readline(prompt = "rate: "))
               rep_exp(rate)
             },
             {
               lambda = as.numeric(readline(prompt = "lambda: "))
               rep_pois(lambda)
             },
             {
               mean = as.numeric(readline(prompt = "mean: "))
               sd = as.numeric(readline(prompt = "sd: "))
               rep_norm(mean, sd)
             },
             {
               shape = as.numeric(readline(prompt = "shape: "))
               scale = as.numeric(readline(prompt = "scale: "))
               rep_weibull(shape, scale)
             },
             {
               meanlog = as.numeric(readline(prompt = "meanlog: "))
               sdlog = as.numeric(readline(prompt = "sdlog: "))
               rep_lnorm(meanlog = meanlog, sdlog)
             },
             {
               df = as.numeric(readline(prompt = "df: "))
               ncp = as.numeric(readline(prompt = "ncp: "))
               rep_student(df, ncp)
             },
             {
               shape = as.numeric(readline(prompt = "shape: "))
               rate = as.numeric(readline(prompt = "rate: "))
               rep_gamma(shape, rate)
             },
             {
               shape1 = as.numeric(readline(prompt = "shape1: "))
               shape2 = as.numeric(readline(prompt = "shape2: "))
               rep_beta(shape1, shape2)
             },
             {
               df = as.numeric(readline(prompt = "df: "))
               ncp = as.numeric(readline(prompt = "ncp: "))
               rep_chisq(df, ncp)
             },
             {
               location = as.numeric(readline(prompt = "location: "))
               scale = as.numeric(readline(prompt = "scale: "))
               rep_cauchy(location, scale)
             }
    )
  }

  functie_random <- function(){
    f <- function(x) {}
    func_text <- readline(prompt = "Introduceti functia:")
    body(f) <- parse(text = func_text)

    interval <- readline(prompt = "Introduceti intervalul (ex: [0,1)): ")

    # pastrez doar numerele si virgula dintre ele
    # fara paranteze, fara spatii, fara nimic altceva
    numeric_only <- strsplit(gsub("[^0-9,.]", "", interval), ",")

    # numeric_only[[1]][1] = primul elem
    # numeric_only[[1]][2] = al doilea elem
    # convertesc stringurile in tipul de data corespunza# tor
    a <- type.convert(numeric_only[[1]][1])
    b <- type.convert(numeric_only[[1]][2])


    # reprezentarea grafica nu accepta valori infinite
    if(is.na(a))
      a <- -5000
    if(is.na(b))
      b <- 5000

    if (grepl("(", interval, fixed=TRUE)){
      a <- a+0.000000001
    }

    if (grepl(")", interval, fixed=TRUE)){
      b <- b-0.000000001
    }

    dx <- density(as.numeric(unlist(lapply(runif(500, a, b), f))))

    plot(dx, lwd = 2, col = "red", main = "Densitate aprox.")

  }

  meniu <- function() {
    cat("1. Meniu Densitati Repartiție\n")
    cat("2. Meniu Funcți de Repartiție\n")
    cat("3. Functie alea# toare.\n")

    option <- as.numeric(readline(prompt = "Optiunea: "))

    # curata consola pentru a nu crea confuzie
    cat("\014")

    switch(EXPR = option,
           afisare_dens(),
           afisare_rep(),
           functie_random()
    )
  }

  meniu()
}

