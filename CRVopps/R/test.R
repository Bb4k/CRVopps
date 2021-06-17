library(CRVopps)


# Comments ----------------------------------------------------------------
# Ctrl + Shift + R = section thing
# -------------------------------------------------------------------------

funcText <- readline(prompt = "Introduceti functia:")

f <- function(x){}

body(f) <- parse(text = funcText)

a <- as.integer(readline(prompt="Low: "))
b <- as.integer(readline(prompt="Hi: "))

# reprezentarea grafica nu accepta valori infinite
if(is.na(a))
  a <- -5000
if(is.na(b))
  b <- 5000


rez  <-  cubintegrate(f, lower = a, upper = b, method = 'pcubature');


ifelse ( is.na(rez$integral) | rez$integral == Inf | rez$integral == -Inf, "Functia introdusa nu are constanta de normalizare", 1/rez$integral )
