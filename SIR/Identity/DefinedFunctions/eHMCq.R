###eHMCq
eHMCq <- function(L_emp,epsilon,M,theta,d,Niter,feature_names,U,grad_U,Leapfrog)
{
  L <- as.numeric(floor(quantile(L_emp, 0.95)))
  Xsim <- matrix(0,nrow=Niter,ncol=d)
  Xsim[1,] <- theta
  
  Comp <- 0
  ratio <- rep(0,Niter)
  ratio_theory <- rep(0,Niter)
  time_eHMCq <- proc.time()
  for(i in 2:Niter)
  {
    r <- sample(1:L,size=1)
    Xstar <- Leapfrog(U,grad_U,epsilon,r,M,Xsim[i-1,])
    Xsim[i,] <- Xstar[1:d]
    ratio[i] <- Xstar[d+1]
    Comp <- Comp + r
    if(!is.na(Xstar[d+2]))
    {
      ratio_theory[i] <- Xstar[d+2]
    }
    else
    {
      ratio_theory[i] <- 0
    }
    if(i %% floor(Niter/10) == 0)
      print(paste("Iteration ",toString(100*round(i/Niter,digits = 2)), "%",sep=""))
  }
  time_eHMCq <- proc.time() - time_eHMCq
  print(c(sum(ratio)/Niter,time_eHMCq))
  
  Xsamp <- Transformation(Xsim,feature_names)
  Xsummary <- SummaryFunction(Xsamp)
  return(list(Xsamp=Xsamp, L=L, Comp=Comp, epsilon=epsilon, Niter=Niter,
              ratio_empirical=mean(ratio), ratio_theory=mean(ratio_theory),
              time=time_eHMCq,Xsummary=Xsummary))
}
