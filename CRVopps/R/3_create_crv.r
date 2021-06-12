library(cubature)

create_crv <- function(){


  type <- readline(prompt="1. Unidim\n2. Bidim\n")

  func_text <- readline(prompt = "Introduceti functia:")

  if(type == 1){

      a <- as.integer(readline(prompt="Low: "))
      b <- as.integer(readline(prompt="Hi: "))

      crv <- c(func_text, a, b)

      return(crv)

  }

  if(type == 2){



  }


}

test <- create_crv()
print(test)
print(test[1])


