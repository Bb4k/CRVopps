#' Pornind de la densitatea comuna a doua variabile aleatoare continue,
#' construirea densitatilor marginale si a densitatilor conditionate
#' @param f Densitatea comuna de probabilitate a doua variabile aleatoare continue.
#' @return Se afiseaza cele doua densitati marginale si cele doua densitati conditionate.
#' @examples
#' dens_marg_cond(f, 0, 1, 0, 1)

dens_marg_cond <- function(f_xy, lo_x, hi_x, lo_y, hi_y){

  library(Ryacas)
  require(Ryacas)


  dens_marg_x <- function(){

    params_1 = paste("x,", lo_y, ",", hi_y, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", trimws(deparse(f)[4])))
    params_2 = paste("x,", -Inf, ",", lo_y, sep="")
    params_3 = paste("x,", hi_y, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", trimws(deparse(f)[7]))))+
      eval(yac_expr(paste("Integrate(", params_3,")", trimws(deparse(f)[7]))))
    cat(sprintf("Densitatea marginala fx:\nExpresia pe prima ramura: %s, %d<y<%d\nExpresia pe a doua a ramura: %s, in rest\n\n\n",
                yac_str(paste("PrettyForm(", first_ramura,")")),lo_y,hi_y,yac_str(paste("PrettyForm(", sec_ramura,")"))))

    ramuri_x <- c(first_ramura, sec_ramura)
    return(ramuri_x)
  }


  dens_marg_y <- function(){

    params_1 = paste("y,", lo_x, ",", hi_x, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", trimws(deparse(f)[4])))
    params_2 = paste("y,", -Inf, ",", lo_x, sep="")
    params_3 = paste("y,", hi_x, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", trimws(deparse(f)[7]))))+
      eval(yac_expr(paste("Integrate(", params_3,")", trimws(deparse(f)[7]))))
    cat(sprintf("Densitatea marginala fy:\nExpresia pe prima ramura: %s, %d<x<%d\nExpresia pe a doua a ramura: %s, in rest\n\n\n",
                yac_str(paste("PrettyForm(", first_ramura,")")),lo_x,hi_x,yac_str(paste("PrettyForm(", sec_ramura,")"))))

    ramuri_y <- c(first_ramura, sec_ramura)
    return(ramuri_y)
  }

  ramuri_x <- dens_marg_x()
  ramuri_y <- dens_marg_y()

  dens_cond_x <- function(){

    cat(sprintf( "Densitatea conditionata fx:\nExpresia pe prima ramura: %s, %d<y<%d",
                 yac_str(paste("PrettyForm(", trimws(deparse(f)[4]),"/",ramuri_x[1],")")),lo_x,hi_x))
    if(ramuri_x[2] != 0)
      cat(sprintf("\n\nExpresia pe a doua a ramura: %s, in rest\n\n\n",
                   yac_str(paste("PrettyForm(", trimws(deparse(f)[7]),"/",ramuri_x[2],")"))))
    else
      cat(sprintf("\n\nExpresia pe a doua a ramura: 0, in rest\n\n\n"))

  }

  dens_cond_y <- function(){

    cat(sprintf( "Densitatea conditionata fy:\nExpresia pe prima ramura: %s, %d<y<%d",
                 yac_str(paste("PrettyForm(", trimws(deparse(f)[4]),"/",ramuri_y[1],")")),lo_y,hi_y))
    if(ramuri_y[2] != 0)
      cat(sprintf("\n\nExpresia pe a doua a ramura: %s, in rest\n\n\n",
                  yac_str(paste("PrettyForm(", trimws(deparse(f)[7]),"/",ramuri_y[2],")"))))
    else
      cat(sprintf("\n\nExpresia pe a doua a ramura: 0, in rest\n\n\n"))

  }

  dens_cond_x()
  dens_cond_y()


}


f <- function(x, y) {
  if(x<=1 && x>=0 && y<=1 && y>=0){
    x+3/2*y^2
  }
  else {
    0
  }

}

dens_marg_cond(f, 0, 1, 0, 1)


