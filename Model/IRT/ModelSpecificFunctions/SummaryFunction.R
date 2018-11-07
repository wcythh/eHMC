###SummaryFunction
SummaryFunction <- function(theta)
{
  N <- nrow(theta)
  
  Index1 <- c(1,102,124)         #log(sigma_theta), log(sigma_a), log(sigma_b)
  Index2 <- 2:101                #theta
  Index3 <- 103:122              #phi   -- log(a)
  Index4 <- 125:144              #b
  Index5 <- c(123,145,146,147)   #mu_b, sigma_theta, sigma_a, sigma_b
  Index6 <- 148:167              #a
  Index7 <- "lp__"

  ess_by_mcmcse <- ess(theta)
  ess_by_coda   <- effectiveSize(theta)
  
  val <- theta[,Index2]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_theta <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index6]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_a <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index4]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_b <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index5]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_hyper <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index7]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)
  
  return(list(ess_by_mcmcse=ess_by_mcmcse, ess_by_coda=ess_by_coda,ESJD_theta=ESJD_theta,
              ESJD_a=ESJD_a, ESJD_b=ESJD_b, ESJD_hyper=ESJD_hyper, ESJD_lp=ESJD_lp))
}
