# Densitati de repartitie -------------------------------------------------

dens_unif <- function(min, max){
  curve(expr = dunif(x = x, min = min, max = max),
        main = "The Uniform Distribution",
        xlab = "x",
        ylab = "f(x)",

        from = -2 * max,
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

        from = -rate,
          to = rate,

        col = "red",
        lwd = 3
  )
}


dens_cauchy <- function(location, scale){
  curve(expr = dcauchy(x = x, location = location, scale = scale),
        main = "The Cauchy Distribution",
        xlab = "x",
        ylab = "f(x)",

        from = location + (-5 * rate),
          to = location + 5 * rate,

        col = "red",
        lwd = 3
  )
}

dens_beta <- function(shape1, shape2){
  curve(expr = dbeta(x = x, shape1 = shape1, shape2 = shape2),
        main = "The Beta Distribution",
        xlab = "x",
        ylab = "f(x)",

        from = 0,
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

        from = 0,
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

        from = exp(meanlog + sdlog) - exp(meanlog + sdlog)*(exp(sdlog)-1),
          to = exp(meanlog + sdlog) + exp(meanlog + sdlog)*(exp(sdlog)-1),

        # media - abaterea = exp(meanlog + 1/2*(sdlog^2)) + exp(2*meanlog + sdlog^2)*(exp(sdlog^2)-1)

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

        from = -3 * max,
          to = 3 * max,

        col = "red",
        lwd = 3
  )
}

rep_norm <- function(mean, sd){
  curve(expr = pnorm(q = x, mean = mean, sd = sd),
        main = "The Normal Distribution Function",
        xlab = "x",
        ylab = "F(x)",

        from = mean + (-2 * sd),
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

        from = 0,
        to = rate * (shape<rate) + shape * (shape>=rate),

        col = "red",
        lwd = 3
  )
}

rep_cauchy <- function(location, scale){
  curve(expr = pcauchy(q = x, location = location, scale = scale),
        main = "The Cauchy Distribution Function",
        xlab = "x",
        ylab = "F(x)",

        from = location + (-5 * rate),
        to = location + 5 * rate,

        col = "red",
        lwd = 3
  )
}

rep_beta <- function(shape1, shape2){
  curve(expr = pbeta(q = x, shape1 = shape1, shape2 = shape2),
        main = "The Beta Distribution Function",
        xlab = "x",
        ylab = "F(x)",

        from = 0,
        to = 1,

        col = "red",
        lwd = 3
  )
}

rep_gamma <- function(shape, rate){
  curve(expr = pgamma(q = x, shape = shape, rate = rate),
        main = "The Gamma Distribution Function",
        xlab = "x",
        ylab = "F(x)",

        from = 0,
        to = rate * (shape<rate) + shape * (shape>=rate),

        col = "red",
        lwd = 3
  )
}

rep_chisq <- function(ncp){
  curve(expr = pchisq(q = x, ncp = ncp),
        main = "The (non-central) Chi-Squared Distribution Function",
        xlab = "x",
        ylab = "F(x)",

        from = 0,
        to = ncp + ncp*df,

        col = "red",
        lwd = 3
  )
}

rep_lnorm <- function(meanlog, sdlog){
  curve(expr = plnorm(q = x, meanlog = meanlog, sdlog = sdlog),
        main = "The Log Normal Distribution Function",
        xlab = "x",
        ylab = "F(x)",

        from = exp(meanlog + sdlog) - exp(meanlog + sdlog)*(exp(sdlog)-1),
        to = exp(meanlog + sdlog) + exp(meanlog + sdlog)*(exp(sdlog)-1),

        col = "red",
        lwd = 3
  )
}

# -------------------------------------------------------------------------

meniu_afisare_densitati <- function() {
  cat("Introduceți numărul corespunzător repartiției dorite.\n")
  cat("1. Uniformă\n")
  cat("2. Normală\n")
  cat("3. Exponențială\n")
  cat("4. Cauchy\n")
  cat("5. Beta\n")
  cat("6. Gamma\n")
  cat("7. Chi-Square\n")
  cat("8. Log-normală\n")
  option <- as.numeric(readline(prompt = "Repartitia aleasă este: "))

  switch(  EXPR = option,
           {
             min = as.numeric(readline(prompt = "min: "))
             max = as.numeric(readline(prompt = "max: "))
             dens_uniforma(min = min, max = max)
           },
           {
             mean = as.numeric(readline(prompt = "mean: "))
             sd = as.numeric(readline(prompt = "sd: "))
             dens_normala(mean = mean, sd = sd)
           },
           {
             rate = as.numeric(readline(prompt = "rate: "))
             dens_exponentiala(rate = rate)
           },
           {
             location = as.numeric(readline(prompt = "location: "))
             scale = as.numeric(readline(prompt = "scale: "))
             dens_cauchy(location = location, scale = scale)
           },
           {
             shape1 = as.numeric(readline(prompt = "shape1: "))
             shape2 = as.numeric(readline(prompt = "shape2: "))
             dens_beta(shape1 = shape1, shape2 = shape2)
           },
           {
             shape = as.numeric(readline(prompt = "shape: "))
             rate = as.numeric(readline(prompt = "rate: "))
             dens_gamma(shape = shape, rate = rate)
           },
           {
             df = as.numeric(readline(prompt = "df: "))
             ncp = as.numeric(readline(prompt = "ncp: "))
             dens_chi_square(ncp = ncp)
           },
           {
             meanlog = as.numeric(readline(prompt = "meanlog: "))
             sdlog = as.numeric(readline(prompt = "sdlog: "))
             dens_log(meanlog = meanlog, sdlog = sdlog)
           }
  )
}

meniu_afisare_functie_repartitie <- function() {
  cat("Introduceți numărul corespunzător repartiției dorite.\n")
  cat("1. Repartiție uniformă\n")
  cat("2. Repartiție normală\n")
  cat("3. Repartiție exponențială\n")
  cat("4. Repartiția Cauchy\n")
  cat("5. Repartiția Beta\n")
  cat("6. Repartiția Gamma\n")
  cat("7. Repartiția Chi-Square\n")
  cat("8. Repartiția Log-normală\n")
  option <- as.numeric(readline(prompt = "Repartitia aleasă este: "))

  switch(  EXPR = option,
           {
             min = as.numeric(readline(prompt = "Min: "))
             max = as.numeric(readline(prompt = "Max: "))
             rep_uniforma(min = min, max = max)
           },
           {
             mean = as.numeric(readline(prompt = "Mean: "))
             sd = as.numeric(readline(prompt = "Sd: "))
             rep_normala(mean = mean, sd = sd)
           },
           {
             rate = as.numeric(readline(prompt = "Rate: "))
             rep_exponentiala(rate = rate)
           },
           {
             location = as.numeric(readline(prompt = "Location: "))
             scale = as.numeric(readline(prompt = "Scale: "))
             rep_cauchy(location = location, scale = scale)
           },
           {
             shape1 = as.numeric(readline(prompt = "Shape1: "))
             shape2 = as.numeric(readline(prompt = "Shape2: "))
             rep_beta(shape1 = shape1, shape2 = shape2)
           },
           {
             shape = as.numeric(readline(prompt = "Shape: "))
             rate = as.numeric(readline(prompt = "Rate: "))
             rep_gamma(shape = shape, rate = rate)
           },
           {
             ncp = as.numeric(readline(prompt = "Ncp: "))
             rep_chi_square(ncp = ncp)
           },
           {
             meanlog = as.numeric(readline(prompt = "Meanlog: "))
             sdlog = as.numeric(readline(prompt = "Sdlog: "))
             rep_log(meanlog = meanlog, sdlog = sdlog)
           }
  )
}

meniu_afisare_grafice <- function() {
  cat("Introduceți un număr din lista de mai jos:\n")
  cat("1. Afișare Densitate Repartiție\n")
  cat("2. Afișare Funcție de Repartiție\n")

  option <- as.numeric(readline(prompt = "Numărul ales este: "))
  switch(EXPR = option,
         meniu_afisare_densitati(),
         meniu_afisare_functie_repartitie()
  )
}

meniu_afisare_grafice()
