###eHMC
eHMC <- function(L_emp,epsilon,M,theta,d,Niter,feature_names,U,grad_U,Leapfrog)
{
  Xsim <- matrix(0,nrow=Niter,ncol=d)
  Xsim[1,] <- theta
  Comp <- 0
  ratio <- rep(0,Niter)
  ratio_theory <- rep(0,Niter)
  time_eHMC <- proc.time()
  for(i in 2:Niter)
  {
    r <- sample(L_emp,size=1)
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
  time_eHMC <- proc.time() - time_eHMC
  print(c(sum(ratio)/Niter,time_eHMC))
  
  Xsamp <- Transformation(Xsim,feature_names)
  Xsummary <- SummaryFunction(Xsamp)
  return(list(Xsamp=Xsamp, Comp=Comp, epsilon=epsilon, Niter=Niter,
              ratio_empirical=mean(ratio), ratio_theory=mean(ratio_theory),
              time=time_eHMC,Xsummary=Xsummary))
}
