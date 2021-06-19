#' Calculul covariantei si coeficientului de corelatie pentru doua variabile aleatoare continue
#' @param f_xy Densitatea comuna de probabilitate a doua variabile aleatoare continue.
#' @param lo_x Valoarea inferioara a domeniului functiei fx
#' @param hi_x Valoarea superioara a domeniului functiei fx
#' @param lo_y Valoarea inferioara a domeniului functiei fy
#' @param hi_y Valoarea superioara a domeniului functiei fy
#' @return Covarianta \code{cov}, coeficientul de corelatie \code{corr}.
#' @examples
#' cov_coef_corel(f, 0, 1, 0, 1)

cov_coef_corel <- function(f_xy, lo_x, hi_x, lo_y, hi_y){

  library(Ryacas)
  require(Ryacas)


  dens_marg_x <- function(){

    params_1 = paste("y,", lo_y, ",", hi_y, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", trimws(deparse(f_xy)[4])))
    params_2 = paste("y,", -Inf, ",", lo_y, sep="")
    params_3 = paste("y,", hi_y, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", trimws(deparse(f_xy)[7]))))+
      eval(yac_expr(paste("Integrate(", params_3,")", trimws(deparse(f_xy)[7]))))

    ramuri_x <- c(first_ramura, sec_ramura)
    return(ramuri_x)
  }

  dens_marg_y <- function(){

    params_1 = paste("x,", lo_x, ",", hi_x, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", trimws(deparse(f_xy)[4])))
    params_2 = paste("x,", -Inf, ",", lo_x, sep="")
    params_3 = paste("x,", hi_x, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", trimws(deparse(f_xy)[7]))))+
      eval(yac_expr(paste("Integrate(", params_3,")", trimws(deparse(f_xy)[7]))))

    ramuri_y <- c(first_ramura, sec_ramura)
    return(ramuri_y)
  }

  ramuri_x <- dens_marg_x()
  ramuri_y <- dens_marg_y()

  media_simpla_x <- function(){

    params_1 = paste("x,", lo_x, ",", hi_x, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", ramuri_x[1],"*x"))
    params_2 = paste("x,", -Inf, ",", lo_x, sep="")
    params_3 = paste("x,", hi_x, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", ramuri_x[2])))+
      eval(yac_expr(paste("Integrate(", params_3,")", ramuri_x[2])))
    return(first_ramura)

  }

  media_simpla_y <- function(){

    params_1 = paste("y,", lo_y, ",", hi_y, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", ramuri_y[1],"*y"))
    params_2 = paste("y,", -Inf, ",", lo_y, sep="")
    params_3 = paste("y,", hi_y, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", ramuri_y[2])))+
      eval(yac_expr(paste("Integrate(", params_3,")", ramuri_y[2])))
    return(first_ramura)

  }

  media_patrata_x <- function(){

    params_1 = paste("x,", lo_x, ",", hi_x, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", ramuri_x[1],"*x*x"))
    params_2 = paste("x,", -Inf, ",", lo_x, sep="")
    params_3 = paste("x,", hi_x, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", ramuri_x[2])))+
      eval(yac_expr(paste("Integrate(", params_3,")", ramuri_x[2])))
    return(first_ramura)
  }

  media_patrata_y <- function(){

    params_1 = paste("y,", lo_y, ",", hi_y, sep="")
    first_ramura <- yac_str(paste("Integrate(", params_1,")", ramuri_y[1],"*y*y"))
    params_2 = paste("y,", -Inf, ",", lo_y, sep="")
    params_3 = paste("y,", hi_y, ",", Inf, sep="")
    sec_ramura <-eval(yac_expr(paste("Integrate(", params_2,")", ramuri_y[2])))+
      eval(yac_expr(paste("Integrate(", params_3,")", ramuri_y[2])))
    return(first_ramura)
  }

  media_xy <- function(){

    params_1 = paste("x,", lo_x, ",", hi_x, sep="")
    first <- yac_str(paste("Integrate(", params_1,")", trimws(deparse(f_xy)[4]),"*x*y"))
    params_2 = paste("y,", lo_y, ",", hi_y, sep="")
    sec <- yac_str(paste("Integrate(", params_2,")", first))
    return(sec)


  }

  ex = media_simpla_x()
  ex2 = media_patrata_x()

  ey = media_simpla_y()
  ey2 = media_patrata_y()


  var_x <- eval(yac_expr(ex2)) - eval(yac_expr(ex)) * eval(yac_expr(ex))
  var_y <- eval(yac_expr(ey2)) - eval(yac_expr(ey)) * eval(yac_expr(ey))

  exy <- media_xy()

  cov <- eval(yac_expr(exy)) - eval(yac_expr(paste(ex,"*",ey)))
  corr <- cov/sqrt(var_y*var_x)

  cat(sprintf("Covarianta este %f, iar coeficientul de corelatie este %f.\n",cov, corr))

}


f <- function(x, y) {
  if(x<1 && x>0 && y<1 && y>0){
    2/3*(2*x+y)
  }
  else {
    0
  }

}

cov_coef_corel(f, 0, 1, 0, 1)


