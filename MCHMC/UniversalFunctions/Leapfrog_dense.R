Leapfrog_dense <- function(U,grad_U,epsilon,L,M,theta, v)
{
  d <- length(theta)
  x_forward <- matrix(0,nrow=L+1,ncol=d)
  v_forward <- matrix(0,nrow=L+1,ncol=d)
  
  x_forward[1,] <- theta
  v_forward[1,] <- v
  
  gradx <- grad_U(x_forward[1,])
  for(ell in 2:(L+1))
  {
    v_forward[ell,] <- v_forward[ell-1,] - 0.5*epsilon*gradx
    x_forward[ell,] <- x_forward[ell-1,] + epsilon*v_forward[ell,]/M
    gradx <- grad_U(x_forward[ell,])
    v_forward[ell,] <- v_forward[ell,] - 0.5*epsilon*gradx
  }
  
  delta <- U(theta) + 0.5*as.numeric(sum(v^2/M)) - U(x_forward[L+1,]) - 0.5*as.numeric(sum(v_forward[L+1,]^2/M))
  
  u <- log(runif(1))
  
  if(u < delta)
  {
    return(list(X=x_forward, V=v_forward, accept=1, rho=delta))
  }
  else
  {
    return(list(X=x_forward, V=v_forward, accept=0, rho=delta))
  }
}
