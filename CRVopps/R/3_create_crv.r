library(cubature)

create_crv <- function(){


  type <- readline(prompt="1. Unidim\n2. Bidim\n")

  funcText <- readline(prompt = "Introduceti functia:")


  f <- function(x){}

  body(f) <- parse(text = funcText)

  if(type == 1){

      a <- as.integer(readline(prompt="Low: "))
      b <- as.integer(readline(prompt="Hi: "))

      crv <- c(f, a, b)

      return(crv)

  }

  if(type == 2){



  }


}

create_crv()



