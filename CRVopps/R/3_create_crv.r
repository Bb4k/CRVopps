library(cubature)

create_crv <- function(){


  type <- readline(prompt="1. Unidim\n2. Bidim\n")

  func_text <- readline(prompt = "Introduceti functia:")

  if(type == 1){

    x_low <- as.integer(readline(prompt="Low: "))
    x_high <- as.integer(readline(prompt="High: "))

    crv <- c(func_text, x_low, x_high)

    return(crv)

  }

  if(type == 2){

    x_low <- as.integer(readline(prompt="x Low: "))
    x_high <- as.integer(readline(prompt="x High: "))
    y_low <- as.integer(readline(prompt="y Low: "))
    y_high <- as.integer(readline(prompt="y High: "))


    crv <- c(func_text, x_low, x_high, y_low, y_high)

    return (crv)
  }


}

test <- create_crv()
print(test)
print(test[1])


