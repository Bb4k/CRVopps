#' Reprezentarea grafică a densității și a funcției de repartiție pentru diferite
#' valori ale parametrilor repartiției. Ȋn cazul ȋn care funcția de repartiție
#' nu este dată ȋntr-o formă explicită (ex. repartiția normală) se acceptă
#' reprezentarea grafică a unei aproximări a acesteia.


# notite ------------------------------------------------------------------
# Vom folosi qnorm si alte astfel de functii pt plotare. Practic
# pt fiecare valoare x, spunem procentul acela, iar plotarea se va face
# singura.
# https://www.statmethods.net/advgraphs/probability.html
# https://gtpb.github.io/ABSTAT18/assets/Day_1/RDistributions.pdf
#
# > x <- rnorm(cate numere sa genereze)


# list_x = vector de valori pe care sa fie aplicata functia f
#
# > dx <- density(as.numeric(unlist(lapply(lista_x, f))))
# > plot(dx, lwd = 2, col = "red", main = "Density")
# > rug(jitter(x))


# runif(cate numere sa genereze, min, max) => numeric vector
# ex: runif(500, -100, 100)
# > dx <- density(as.numeric(unlist(lapply(runif(500, a, b), f))))

# -------------------------------------------------------------------------



# constructor pentru functii
make_func <- function(x){
  x;
  function() x
}

# voi face o lista de functii
func_list <- list()

# vectori auxiliari pt intervalele
# vector_low <- c()
# vector_hi <- c()

# Primesc de la tastatura nr de ramuri, pentru a face "alocarile" dinamic
n <- readline(prompt = "Cate ramuri are functia:")

for (i in 1:n){
  # anunt pt ce ramura introducem date
  sprintf("Ramura %d:", n)

  # citesc functia si o parsez pe pozitia i din lista de functii
  func_text <- readline(prompt = "Introduceti functia:")
  body(func_list[[i]]) <- parse(text = func_text)

  interval <- readline(prompt = "Introduceti intervalul (ex: [0,1)): ")

  # pastrez doar numerele si virgula dintre ele
  # fara paranteze, fara spatii, fara nimic altceva
  numeric_only <- strsplit(gsub("[^0-9,.]", "", interval), ",")

  # numeric_only[[1]][1] = primul elem
  # numeric_only[[1]][2] = al doilea elem
  # convertesc stringurile in tipul de data corespunzator
  a <- type.convert(numeric_only[[1]][1])
  b <- type.convert(numeric_only[[1]][2])


  # reprezentarea grafica nu accepta valori infinite
  if(is.na(a))
    a <- -5000
  if(is.na(b))
    b <- 5000

  if (grepl("(", interval)){
    a <- a-1
  }

  if (grepl(")", interval)){
    b <- b-1
  }


  # le adaug in listele aferente
  # astfel, pozitia i din toate cele 5 liste pe care le cream
  # va determina functia corespunzatoare ramurei i
  vector_low <- c(vector_low, a)
  vector_hi  <- c(vector_hi, b)




  # curata consola pentru a nu crea confuzie
  cat("\014")
}

# transform vectorii auxiliari pt intervale intr-un
# tabel cu 4 coloane. Format:
#      [,1] [,2]   [,3]   [,4]
# [1,] "["  "3.12" "17"   ")"
# [2,] "["  "0"    "1"    ")"
# [3,] "("  "1"    "3"    "]"
# [4,] "["  "4.44" "5.55" "]"
interval_array <- array(c(vector_st,vector_low,vector_hi,vector_dr),dim = c(n,4))

