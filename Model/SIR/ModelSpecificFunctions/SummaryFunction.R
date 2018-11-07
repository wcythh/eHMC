###SummaryFunction
SummaryFunction <- function(theta)
{
  N <- nrow(theta)
  ess_by_mcmcse <- ess(theta)
  ess_by_coda   <- effectiveSize(theta)
  
  ESJD_para_log <- theta[-1,1:4] - theta[-N,1:4]
  ESJD_para_log <- sum(apply(ESJD_para_log,1,function(xx) sum(xx^2)))
  
  ESJD_para <- theta[-1,5:8] - theta[-N,5:8]
  ESJD_para <- sum(apply(ESJD_para,1,function(xx) sum(xx^2)))
  
  ESJD_lp <- theta[-1,89] - theta[-N,89]
  ESJD_lp <- sum(ESJD_lp^2)
  
  return(list(ess_by_mcmcse=ess_by_mcmcse, ess_by_coda=ess_by_coda,ESJD_para_log=ESJD_para_log,
              ESJD_para=ESJD_para, ESJD_lp=ESJD_lp))
}
