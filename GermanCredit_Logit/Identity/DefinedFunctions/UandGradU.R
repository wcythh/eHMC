U <- function(x)
{
  val <- as.vector(R0 %*% x)
  return(sum(log(1+exp(val)) - S0*val))
}

grad_U <- function(x)
{
  val <- as.vector(R0 %*% x)
  val <- 1/(1+exp(-val)) - S0
  val <- as.vector(t(R0) %*% val)
  return(val)
}
