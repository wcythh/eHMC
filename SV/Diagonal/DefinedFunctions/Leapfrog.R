Leapfrog <- function(U,grad_U,epsilon,L,M,current_q)
{
  ### Based on the Neal's implementation ###
  q = current_q
  p = rnorm(length(q)) * sqrt(M)
  current_p = p
  p = p - 0.5*epsilon * as.vector(grad_U(q))
  
  for(l in 1:L)
  {
    q = q + epsilon * as.vector(p/M)
    if(l != L)
      p = p - epsilon * as.vector(grad_U(q))
  }
  p = p - 0.5*epsilon * grad_U(q)
  p = -p
  current_U <- U(current_q)
  current_K <- 0.5*as.numeric(sum(current_p^2/M))
  proposed_U <- U(q)
  proposed_K <- 0.5*as.numeric(sum(p^2/M))
  rho <- exp(current_U-proposed_U+current_K-proposed_K)
  if(is.na(rho))
  {
    return(c(current_q,0,NA))
  }
  else
  {
    if(runif(1) < rho)
    {
      return(c(q,1,rho))
    }
    else
    {
      return(c(current_q,0,rho))
    }
  }
}
