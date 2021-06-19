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
  cat("Variabila aleatoare X urmeaza legea Weibull cu parametrii λ si k, daca densitatea
      sa de repartitie este functia:\n")
  cat("\n")
  cat("             | k/λ * (x/λ)^(k-1) * exp(-(x/λ)^k), x>=0\n")
  cat("      f(x) = |                                  , λ>0, k>0\n")
  cat("             |                 0                , x<0\n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  shape = k = determina forma figurii \n")
  cat("            scale = λ = ingusteaza curba \n")
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
  cat("    (in R)  rate = λ = accentueaza curba \n")
  cat("\n")
  cat("     Media: λ \n")
  cat(" Dispersia: 2 \n")
  cat("     Range: [μ, Inf)   \n")
  cat("     Folos: Este utilizata pentru a modela informatii cu o rata constanta de esec
            (generate de dezastre naturale).\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3667.htm\n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
}

Log_Normal <- function(){
  cat("Variabila aleatoare X urmeaza legea log-normala cu parametrii m si σ (m∈R, σ>0),
      daca densitatea sa de repartitie este functia:\n")
  cat("\n")
  cat("             | 1/(σ * x * sqrt(s*pi)) * exp(-(lnx - m)^2/(2σ^2) ), x>0\n")
  cat("f(x; m, σ) = |                                  , \n")
  cat("             |                 0                , x<=0\n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  meanlog = media valorilor generate de x\n")
  cat("            sdlog = deviatia standard\n")
  cat("\n")
  cat("     Media: exp(0.5 * σ^2)")
  cat(" Dispersia: sqrt(exp(σ^2) * (exp(σ^2) -1))")
  cat("     Range: (0,Inf)\n")
  cat("     Folos: Precum densitatea Weibull, este utilizata pe scara larga in aplicatii\n")
  cat("            de prezicere a unor evenimente ce presupun moartea unui organism.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3669.htm\n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
}

Gamma <- function(){
  cat("Variabila aleatoare X urmeaza legea gamma cu parametrul p (p>0),
      daca densitatea sa de repartitie este functia:\n")
  cat("\n")
  cat("             | x^(p-1) * exp(-x)/Gamma(p), x>0\n")
  cat("      f(x) = |                                   \n")
  cat("             |                 0                , x<=0 \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  rate = lateste curba \n")
  cat("            shape = determina forma figurii \n")
  cat("\n")
  cat("     Media: p")
  cat(" Dispersia: sqrt(p)")
  cat("     Range: (0,Inf)\n")
  cat("     Folos: Genereaza maximul de entropie intr-o distributie de probabilitati.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda366b.htm\n")
  cat("            https://web.archive.org/web/20160307144515/http://wise.xmu.edu.cn/uploadfiles/paper-masterdownload/2009519932327055475115776.pdf\n")
}

Beta <- function(){
  cat("Variabila aleatoare X urmeaza legea beta cu parametrii p si q (p,q>0),
      daca densitatea sa de repartitie este functia:\n")
  cat("\n")
  cat("             | x^(p-1) * (1-x)^(q-1) / B(p,q), x∈(0,1), B=fct. beta a lui Euler\n")
  cat("      f(x) = |                                \n")
  cat("             |                 0             , x∉(0,1) \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  shape1, shape2 = determina forma figurii \n")
  cat("\n")
  cat("     Media: shape1/(shape1 + shape2)")
  cat(" Dispersia: shoape1 * shape2 / ((shape1 + shape2)^2 * (shape1 + shape2 + 1))")
  cat("     Range: (0,1)\n")
  cat("     Folos: Este folosita pentru a proiecta evenimente cu un range restrans, precum (0,1).\n")
  cat("\n\n")
  cat("     Sursa: http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf\n")
  cat("            documentatia din R\n")
}

Normal <- function(){
  cat("Variabila aleatoare X urmeaza legea normala cu parametrii m si σ (σ>0, m∈R),
      daca densitatea sa de repartitie este functia:\n")
  cat("\n")
  cat("f(x; m, σ) = exp(-(x-m)^2/(2 * σ^2)) / (σ * sqrt(2 * pi)) , x∈R \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii;\n")
  cat("    (in R)  mean = media valorilor generate de x\n")
  cat("            sd = deviatia standard\n")
  cat("\n")
  cat("     Media: mean")
  cat(" Dispersia: sd")
  cat("     Range: (-Inf,Inf)\n")
  cat("     Folos: Multe teste statistice pleaca de la ideea ca informatiile urmaresc\n")
  cat("            o repartitie normala. Acest lucru trebuie testat inainte oricaror alte teste.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda3661.htm.htm \n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf \n")
}

Binomial <- function(){
  cat("Variabila aleatoare X urmeaza legea binomiala cu parametrii n si p (0<p<1, n∈N),
      daca ia valorile 0,1,2...,n cu probabilitatile:\n")
  cat("\n")
  cat("     p(x) = C(n,x)*p^x*(1-p)^(n-x) , x∈{0,1,..,n} \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii; \n")
  cat("    (in R)  prob = p = sansa de succes la un experiment \n")
  cat("            size = n = nr de esecuri pana cand experimentul este oprit \n")
  cat("            q = sansa de a esua la un experimente \n")
  cat("\n")
  cat("     Media: n*p")
  cat(" Dispersia: sqrt((1-p)/(n*p)) \n")
  cat("     Range: (0,n)\n")
  cat("     Folos: Rezolva cazurile in care exista doar 2 outcome-uri posibile.\n")
  cat("\n\n")
  cat("     Sursa: https://www.itl.nist.gov/div898/handbook/eda/section3/eda366i.htm \n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf \n")
}

Bernoulli <- function(){
  cat("Pentru n=1, legea binomiala este cunoscuta si sub numele de legea Bernoulli cu param p (0<p<1, n∈N).
      Variabilele aleatoare X care urmeaza legea Bernoulli cu param. o admite doar 2 valori
      posibilie, 0 si 1 cu probabilitatile de realizare q=1-p si p, avand tabloul repartitiei:\n")
  cat("\n")
  cat("     p(x) = C(n,x)*p^x*(1-p)^(n-x) , x∈{0,1,..,n} \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii; \n")
  cat("    (in R)  prob = p = sansa de succes la un experiment \n")
  cat("            q = sansa de a esua la un experimente \n")
  cat("\n")
  cat("     Media: {0,1}")
  cat(" Dispersia: p * (1-p) = p * q \n")
  cat("     Range: (0,1)\n")
  cat("     Folos: Rezolva cazurile in care exista doar 2 outcome-uri posibile.\n")
  cat("\n\n")
  cat("     Sursa: documentatia R \n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf \n")
}


Negative_Binomial <- function(){
  cat("Variabila aleatoare X urmeaza legea binomiala cu exponent negativ cu parametrii n si p (0<p<1, n∈N*),
      daca densitatea sa de repartitie este functia:\n")
  cat("\n")
  cat("     f(x) = Gamma(x+n)/(Gamma(n) * x!) * p^n * (1-p)^x, x∈{0,1,..,n} \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii; \n")
  cat("    (in R)  prob = p = sansa de succes de la fiecare experiment \n")
  cat("            size = n = nr de esecuri pana cand experimentul este oprit \n")
  cat("\n")
  cat("     Media: p * n / (1-p)")
  cat(" Dispersia: p * n / (1-p)^2")
  cat("     Range: (-Inf,Inf)\n")
  cat("     Folos: Multe teste statistice pleaca de la ideea ca informatiile urmaresc\n")
  cat("            o repartitie normala. Acest lucru trebuie testat inainte oricaror alte teste.\n")
  cat("\n\n")
  cat("     Sursa: https://mathvault.ca/hub/higher-math/math-symbols/probability-statistics-symbols/ \n")
  cat("            http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf \n")
}

Geometric <- function(){
  cat("Numim variabila aleatoare X geometrica cu parametrul p (p∈(0,1)), o var. aleatoare X
      reprezentand numarul de incercari efectuate intr-un sir de experimente Bernoulli
      independente, cu acelasi parametru p, pana la aparitia primului succes, adica:\n")
  cat("\n")
  cat("     p(x) = p(1-p)^x, x∈{0,1,2,..} \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii; \n")
  cat("    (in R)  prob = p = sansa de succes de la fiecare experiment \n")
  cat("\n")
  cat("     Media: 1/p")
  cat(" Dispersia: sqrt((1-p)/(p^2))")
  cat("     Range: (-Inf,Inf)\n")
  cat("     Folos: Ajuta la anticiparea numarului de esecuri avute pana la intalnirea\n")
  cat("            primului succes.\n")
  cat("\n\n")
  cat("     Sursa: http://cs.unitbv.ro/~pascu/stat/Distributii%20discrete%20clasice.pdf  \n")
}

Hypergeometric <- function(){
  cat("Fie X variabila aleatoare care ia ca valori numarul de bile albe care se obtin extragand
      n bile dintr-o urna care contine a bile albe si b bile negre (n<=a+b). Tabloul de repartite
      al variabilei aleatoare X este:\n")
  cat("\n")
  cat("     p(x) = C(m,x) * C(n, k-x) / C(m+n, k), x = {0, 1, ..., k} \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii; \n")
  cat("    (in R)  m = a (bile albe)\n")
  cat("            n = b (bile negre)\n")
  cat("            k = n (bile extrase)\n")
  cat("\n")
  cat("     Media: k * m / (m+n)")
  cat(" Dispersia: k * p * (1-p) * (m+n-k)/(m+n-1)")
  cat("     Range: (-Inf,Inf)\n")
  cat("     Folos: Poate fi folosit pentru determina sansa pe alege o piesa stricata dintr-o cutie,\n")
  cat("            fara a pune piese inapoi).\n")
  cat("\n\n")
  cat("     Sursa: http://math.etc.tuiasi.ro:81/rosu/didactic/MS%20II_Curs_Variabile%20aleatoare%20discrete_II.pdf  \n")
  cat("            https://docs.oracle.com/cd/E57185_01/CYBUG/hypergeometric_distribution.htm\n")
}

Poisson <- function(){
  cat("Variabila aleatoare X urmeaza legea Poisson cu parametrul λ (λ>0),
      daca poate lua orice valoare intreaga pozitiva si:\n")
  cat("\n")
  cat("     p(x) = λ^x * exp(-λ)/x, x∈{0,1,2,...} \n")
  cat("\n")
  cat("Parametrii: x = fct. cu care este generat vectorul de numere ale multimii; \n")
  cat("    (in R)  lambda = vector de medii (means) \n")
  cat("\n")
  cat("     Media: λ")
  cat(" Dispersia: λ")
  cat("     Range: (0,Inf)\n")
  cat("     Folos: Poate prezice de cate ori poate un eveniment sa aibe loc intr-o\n")
  cat("            perioada specifica de timp.\n")
  cat("\n\n")
  cat("     Sursa: http://dep2.mathem.pub.ro/pdf/didactice/Probabilitati%20si%20statistica.pdf \n")
  cat("            documentatia din R \n")
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
