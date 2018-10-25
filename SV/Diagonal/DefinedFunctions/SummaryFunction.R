###SummaryFunction
SummaryFunction <- function(theta)
{
  N <- nrow(theta)
  
  Index1 <- 1:1000
  Index2 <- 1001:1003
  Index3 <- 1004:1006
  Index4 <- "lp__"

  ess_by_mcmcse <- ess(theta)
  ess_by_coda   <- effectiveSize(theta)
  
  val <- theta[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_x <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index2]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_log <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index3]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_para <- sum(apply(val,1,function(xx) sum(xx^2)))
  
  val <- theta[,Index4]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)
  
  return(list(ess_by_mcmcse=ess_by_mcmcse, ess_by_coda=ess_by_coda,ESJD_x=ESJD_x,
              ESJD_log=ESJD_log, ESJD_para=ESJD_para, ESJD_lp=ESJD_lp))
}
