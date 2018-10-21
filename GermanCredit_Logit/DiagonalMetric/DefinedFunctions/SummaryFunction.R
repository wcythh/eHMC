###SummaryFunction
SummaryFunction <- function(theta)
{
  N <- nrow(theta)
  ess_by_mcmcse <- ess(theta)
  ess_by_coda   <- effectiveSize(theta)
  
  ESJD_para <- theta[-1,1:25] - theta[-N,1:25]
  ESJD_para <- sum(apply(ESJD_para,1,function(xx) sum(xx^2)))
  
  ESJD_lp <- theta[-1,26] - theta[-N,26]
  ESJD_lp <- sum(ESJD_lp^2)
  
  return(list(ess_by_mcmcse=ess_by_mcmcse, ess_by_coda=ess_by_coda,
              ESJD_para=ESJD_para, ESJD_lp=ESJD_lp))
}
