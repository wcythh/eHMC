###LongestBatch
LongestBatch <- function(U,grad_U,epsilon,L,M,x)
{
  current_x <- x
  current_v <- rnorm(length(x)) * sqrt(M)
  proposed_x <- current_x
  proposed_v <- current_v
  
  candidate_x <- current_x
  candidate_v <- current_v
  
  Increment <- 0
  Stop <- 0
  StopInd <- 0
  ell <- 0
  
  while(Increment >= 0 | ell < L)
  {
    ell <- ell + 1
    proposed_v <- proposed_v - 0.5*epsilon*grad_U(proposed_x)
    proposed_x <- proposed_x + epsilon*as.vector(proposed_v/M)
    proposed_v <- proposed_v - 0.5*epsilon*grad_U(proposed_x)
    if(Stop == 0)
    {
      Delta_x <- proposed_x - current_x
      Increment <- sum(Delta_x * proposed_v/M)
      if(is.na(Increment))
      {
        return(c(current_x, 0, NA, ell, ell))
      }
      if(Increment < 0)
      {
        StopInd <- ell
        Stop <- 1
      }
    }
    if(ell == L)
    {
      candidate_x <- proposed_x
      candidate_v <- proposed_v
    }
    if(ell > 1e4)
    {
      return(c(current_x, 0, NA, ell, ell))
    }
  }
  
  current_U <- U(current_x)
  current_K <- 0.5*as.numeric(sum(current_v^2/M))
  candidate_U <- U(candidate_x)
  candidate_K <- 0.5*as.numeric(sum(candidate_v^2/M))
  
  rho <- current_U + current_K - candidate_U - candidate_K
  if(is.na(rho))
  {
    return(c(current_x, 0, NA, NA, 0))
  }
  else
  {
    u <- log(runif(1))
    if(u < rho)
    {
      return(c(candidate_x, 1, rho, StopInd, ell))
    }
    else
    {
      return(c(current_x, 0, rho, StopInd, ell))
    }
  }
}
