###LearnL
LearnL <- function(epsilon,M,theta,d,Niter,feature_names,U,grad_U,LongestBatch)
{
  Xsim <- matrix(0,nrow=Niter,ncol=d)  #samples
  Xsim[1,] <- theta
  L_0 <- 10                            #initial step size, L_0
  L_emp <- L_0                         #record empirical distribuion of local longest batches
  ratio <- 0                           #accept ratio in samples
  Comp <- 0                            #Number of gradient for HMC
  Comp_Random <- 0                     #Number of gradient for LongestBatch
  ratio_theory <- 0                    #accept ratio in theory
  time_L <- proc.time()                #program running time
  for(i in 2:Niter)
  {
    L <- as.numeric(quantile(L_emp,0.95)) #update upper bound of leapfrog based on generated empirical 
    L_r <- sample(1:L,size=1)             #Leapfrog steps
    Xstar <- LongestBatch(U,grad_U,epsilon,L_r,M,Xsim[i-1,])
    Xsim[i,] <- Xstar[1:d]
    ratio <- ratio + Xstar[d+1]
    Comp <- Comp + 2*Xstar[(d+4)] + 1
    Comp_Random <- Comp_Random + 2*L_r+1
    if(!is.na(Xstar[d+2]))                #not diverge
    {
      ratio_theory <- ratio_theory + exp(min(c(0,Xstar[(d+2)])))
      L_emp <- c(L_emp, Xstar[d+3])
    }
    else                               #diverge
    {
      ratio_theory <- ratio_theory + 0
    }
    if(i %% floor(Niter/10) == 0)
      print(paste("Iteration ",toString(100*round(i/Niter,digits = 2)), "%",sep=""))
  }
  time_L <- proc.time() - time_L
  print(round(c(ratio/Niter,time_L),digits = 4))
  
  Xsamp_LearnL <- Transformation(Xsim, feature_names)
  return(list(epsilon=epsilon, Niter=Niter, L_emp=L_emp,
              Xsamp=Xsamp_LearnL, time=time_L, ratio=ratio/Niter, 
              ratio_theory=ratio_theory/Niter))
}
