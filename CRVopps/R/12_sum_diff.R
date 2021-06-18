library(pracma)

convolution_sum <- function(fx,fy) {
  function(z) {
    integrate(function(y) {
      fx(z-y) * fy(y)
    },-Inf,Inf)
  } $ value
}


convolution_diff <- function(fx,fy) {
  function(z) {
    integrate(function(y) {
      fx(y-z)*fy(y)
    },-Inf,Inf)
  } $ value
}


f <- function(x)(dnorm(x,1)) #distributie normala cu mean = 1
g <- function(x) (dnorm(x,2))
sum <- Vectorize(convolution_sum(f, g))
sub <- Vectorize(convolution_diff(f, g))


par(mfrow=c(0,2))
plot(sum,from=-5,to=6,type="l")
plot(sub,from=-5,to=6,type="l")
