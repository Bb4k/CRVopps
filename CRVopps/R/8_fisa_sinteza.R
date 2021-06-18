#' @name Fisa de Sinteza
#'
#' Acesta functie afiseaza informatii generale despre toate tipurile de densitati de repartitii
#' implementate la cerinta 4: uniforme, Bernoulli, binomiale (si negativ binomiale),
#' geometrice, hypergeometrice, exponentiale, Poisson, normale, log-normale.
#' Weibull, Student's T, gamma, beta, Chi-Square si Cauchy. In # total 16 tipuri.


Uniform <- function(){
  cat("Spunem că variabila aleatoare X este repartizată uniform pe intervalul
[a,b], a<b, dacă ea are densitatea:\n")
  cat("\n")
  cat("       | 1/(b-a), x∈[a,b]\n")
  cat("f(x) = |\n")
  cat("       |    0   , în caz contrar.\n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  min, max = limitele minime si maxime ale distributiei (finite);\n")
  cat("\n")
  cat("     Media: (a+b)/2\n")
  cat(" Dispersia: sqrt((b-a)^2/12)\n")
  cat("     Range: b-a\n")
  cat("     Folos: Una dintre cele mai importante aplicatii ale acestei densitati este\n")
  cat("            in generarea numerelor aleatoare.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3662.htm\n")
}

Weibull <- function(){
  cat("Variabila aleatoare X urmeaza legea Weibull cu parametrii λ si α, daca densitatea
      sa de repartitie este functia:\n")
  cat("\n")
  cat("             | k/λ * (x/λ)^(k-1) * exp(-(x/λ)^k), x>=0\n")
  cat("      f(x) = |                                  , λ>0, k>0\n")
  cat("             |                 0                , x<0\n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  shape = k  \n")
  cat("            scale = λ  \n")
  cat("\n")
  cat("     Media: λ * Gamma(1 + 1/k)")
  cat(" Dispersia: λ^2 * [Gamma(1+2/k) - Gamma(1+1/k)^2]\n")
  cat("     Range: [0,Inf)\n")
  cat("     Folos: Densitatea Weibull este utilizata pe scara larga in aplicatii\n")
  cat("            de prezicere a unor evenimente ce presupun moartea unui organism.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3668.htm\n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
}

Student <- function(){
  cat("Variabila aleatoare X urmeaza legea Weibull cu parametrul n (n ∈ N*), daca densitatea
      sa de probabilitate este functia:\n")
  cat("\n")
  cat("      f(x) = Gamma((n+1)/2)/sqrt(n*pi)/Gamma(n/2) * (1 + x^2/n)^(-(n+1)/2),  x ∈ R  \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  df = n = gradele de libertate permise\n")
  cat("            ncp = element de descentrare \n")
  cat("\n")
  cat("     Media: 0\n")
  cat(" Dispersia: sqrt(v/(v-2)) \n")
  cat("     Range: (-Inf, Inf)   \n")
  cat("     Folos: Densitatea t este utilizata in multe cazuri pentru punctele slabe\n")
  cat("            ale speculatiilor, determinand intervale de valori pe care se aplica
            acestea.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3664.htm\n")
}

Cauchy <- function(){
  cat("Pentru n = 1, repartitia Student se mai numeste legea Cauchy. O variabila aleatoare X
      cu repartitie Cauchy are densitatea de probabilitate functia:\n")
  cat("\n")
  cat("      f(x) = 1 / (pi * (1+x^2)),  x ∈ R  \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  location = element de descentrare \n")
  cat("            scale = lateste graficul\n")
  cat("\n")
  cat("     Media: nedefinita \n")
  cat(" Dispersia: nedefinita \n")
  cat("     Range: (-Inf, Inf)   \n")
  cat("     Folos: Sunt foarte folositoare in verificarea unor tehnici robuste, sa vedem \n")
  cat("            cum se cumporta cand lucreaza cu o gama variata de valori (codite).\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3663.htm\n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
}

Chi_Square <- function(){
  cat("O distributie chi-square este rezultatul a v variabile independente cu densitati standard
      sunt ridicate la patrat si adunate. Functia este:\n")
  cat("\n")
  cat("      f(x) = (exp(-x/2) * x^(v/2-1))/(2^(v/2) * Gamma(v/2)),  x >= 0  \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  df = gradele de libertate permise\n")
  cat("            ncp = element de descentrare \n")
  cat("\n")
  cat("     Media: v \n")
  cat(" Dispersia: sqrt(2/v) \n")
  cat("     Range: [0, Inf)   \n")
  cat("     Folos: Densitatea t este utilizata in multe cazuri pentru punctele slabe\n")
  cat("            ale speculatiilor, determinand intervale de valori pe care se aplica
            acestea.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3666.htm\n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
}

Exponential <- function(){
  cat("Pentru α = 1, repartitia Weibull se mai numeste legea exponentiala de parametru λ.
     Deci densitatea de probabilitate a unei variabile aleatoare X cu repartitie exp. cu param λ este:\n")
  cat("\n")
  cat("             | λ * exp(-λ * x),  x > 0  \n")
  cat("      f(x) = | \n")
  cat("             |        0       ,  x <= 0  \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  df = gradele de libertate permise\n")
  cat("            ncp = element de descentrare \n")
  cat("\n")
  cat("     Media: v \n")
  cat(" Dispersia: sqrt(2/v) \n")
  cat("     Range: [0, Inf)   \n")
  cat("     Folos: Densitatea t este utilizata in multe cazuri pentru punctele slabe\n")
  cat("            ale speculatiilor, determinand intervale de valori pe care se aplica
            acestea.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3666.htm\n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
}

meniu <- function() {
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
  cat("12. Student\n")
  cat("13. Gamma\n")
  cat("14. Beta\n")
  cat("15. Chi-Square\n")
  cat("16. Cauchy\n")

  option <- as.numeric(readline(prompt = "Optiunea: "))

  # curata consola pentru a nu crea confuzie
  cat("\014")

  switch(EXPR = option,
         Uniform(),
         Bernoulli(),
         Binomial(),
         Negative_Binomial(),
         Geometric(),
         Hypergeometric(),
         Exponential(),
         Poisson(),
         Normal(),
         Weibull(),
         Log_Normal(),
         Student(),
         Gamma(),
         Beta(),
         Chi_Square(),
         Cauchy()
  )
}

meniu()
