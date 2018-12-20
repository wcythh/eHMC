U <- function(x)
{
  return(as.numeric(0.5 * t(x)%*% A_Inv %*% x))
}

grad_U <- function(x)
{
  return(as.vector(A_Inv %*% x))
}
