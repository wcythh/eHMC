library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)

MixSampler <- function(zz)
{
  set.seed(zz*1e7)
  Index1 <- 1:100
  Index2 <- "lp__"
  
  SummaryResult <- matrix(0,nrow=4,ncol=11)
  ###################### NUTS ######################
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  
  d <- 100
  mu <- rep(0,d)
  A <- matrix(0,nrow=d,ncol=d)
  for(i in 1:d)
  {
    for(j in 1:d)
      A[i,j] <- 0.99^(abs(i-j))
  }
  A_Inv <- solve(A)
  init0 <- mvrnorm(1,mu=mu,Sigma=A)
  data = list(d=d,mu=mu,A=A)
  
  iter0 <- 25e3
  warmup0 <- 5e3
  fit <- stan("MVN.stan",
              data=data, chains=1, iter=iter0, warmup = warmup0,
              init = init0,save_dso = TRUE,
              verbose=FALSE,
              control = list(adapt_engaged=TRUE,stepsize=0.01,
                             metric="unit_e",adapt_delta=0.8,max_treedepth=14),
              algorithm = "NUTS")
  
  sampleNUTS <- as.matrix(fit)
  result <- fit@sim$samples
  L_trace <- attr(result[[1]],"sampler_params")$n_leapfrog__
  stepsize_trace <- attr(result[[1]],"sampler_params")$stepsize__
  accept_trace <- attr(result[[1]],"sampler_params")$accept_stat__
  depth_trace <- attr(result[[1]],"sampler_params")$treedepth__
  epsilon0 <- stepsize_trace[iter0]
  CompNUTS <- sum((2*L_trace[-c(1:warmup0)]+1))
  
  val <- sampleNUTS[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_para <- sum(apply(val,1,function(xx) sum(xx^2)))/CompNUTS
  
  val <- sampleNUTS[,Index2]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/CompNUTS
  
  val_ess <- ess(sampleNUTS[,1:100])/CompNUTS
  SummaryResult[1,] <- c(min(val_ess), mean(val_ess), median(val_ess), max(val_ess),ess(sampleNUTS[,"lp__"])/CompNUTS,ESJD_para, ESJD_lp, CompNUTS, mean(L_trace[-c(1:warmup0)]), epsilon0, nrow(sampleNUTS))
  
  NUTSnames <- colnames(sampleNUTS)
  rm(fit)
  rm(result)
  rm(sampleNUTS)
  ###################### functions ######################
  U <- function(x)
  {
    return(0.5*as.numeric(t(x-mu)%*%A_Inv%*%(x-mu)))
  }
  
  grad_U <- function(x)
  {
    return(as.vector(A_Inv%*%(x-mu)))
  }
  ###################### Computing longest batches ######################
  HMC1 <- function(U,grad_U,epsilon,L,x)
  {
    current_x <- x
    current_v <- rnorm(length(x))
    
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
      proposed_x <- proposed_x + epsilon*proposed_v
      proposed_v <- proposed_v - 0.5*epsilon*grad_U(proposed_x)
      if(Stop == 0)
      {
        Delta_x <- proposed_x - current_x
        Increment <- sum(Delta_x * proposed_v)
        if(is.na(Increment))
        {
          return(c(current_x, 0, NA, NA, 0))
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
    }
    
    current_U <- U(current_x)
    current_K <- 0.5*sum(current_v^2)
    candidate_U <- U(candidate_x)
    candidate_K <- 0.5*sum(candidate_v^2)
    
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
  ###################### classical HMC Sampler ######################
  HMC <- function(U,grad_U,epsilon,L,current_q)
  {
    q = current_q
    p = rnorm(length(q),0,1)
    current_p = p
    p = p - 0.5*epsilon * as.vector(grad_U(q))
    for(l in 1:L)
    {
      q = q + epsilon * p
      if(l != L)
        p = p - epsilon * as.vector(grad_U(q))
    }
    p = p - 0.5*epsilon * as.vector(grad_U(q))
    p = -p
    current_U <- U(current_q)
    current_K <- sum(current_p^2)/2
    proposed_U <- U(q)
    proposed_K <- sum(p^2)/2
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
  
  ###################### generating empirical distribution ######################
  ChooseL <- function(epsilon, theta, Niter)
  {
    #d <- 25
    Xsim <- matrix(0,nrow=Niter,ncol=d)
    Xsim[1,] <- theta
    #print(Xsim[1,])
    #Xsim[1,] <- log(c(1.0120e+00,1.9979e-01,1.0546e+01,3.5613e-01))
    L <- 10
    Ind_trace <- L
    ratio <- 0
    Comp <- 0
    Comp_Random <- 0
    ratio_theory <- 0
    time_L <- proc.time()
    i <- 1
    while(i < Niter)
    {
      L <- quantile(Ind_trace,0.95)
      L_r <- sample(1:L,size=1)
      Xstar <- HMC1(U,grad_U,epsilon,L_r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i+1
        Xsim[i,] <- Xstar[1:d]
        ratio <- ratio + Xstar[d+1]
        ratio_theory <- ratio_theory + exp(min(c(0,Xstar[(d+2)])))
        Ind_trace <- c(Ind_trace, Xstar[d+3])
        Comp <- Comp + 2*Xstar[(d+4)] + 1
        Comp_Random <- Comp_Random + 2*L_r+1
        #print(round(c(i,Xsim[i,1:d],Ind_trace[length(Ind_trace)],length(Ind_trace),L_r),digits=4))
      }
      if(i %% floor(Niter/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter,digits = 2)), "%",sep=""))
    }
    time_L <- proc.time() - time_L
    Ind_trace <- Ind_trace[which(!is.na(Ind_trace))]
    print(round(c(ratio/Niter,length(Ind_trace),time_L),digits = 4))
    
    return(list(epsilon=epsilon, Niter=Niter, Ind_trace=Ind_trace,
                Xsim=Xsim, time=time_L, ratio=ratio/Niter, ratio_theory=ratio_theory/Niter))
  }
  ###################### eHMCq ######################
  eHMCq <- function(Ind_trace,Niter,epsilon,theta)
  {
    #d <- 25
    L <- as.numeric(floor(quantile(Ind_trace, 0.95)))
    #Niter_hmc <- floor(CompNUTS/(0.5*L+2.5))
    Niter_hmc <- Niter
    print(c(L, Niter_hmc))
    Xsim <- matrix(0,nrow=Niter_hmc,ncol=d)
    Xsim[1,] <- theta
    
    Comp <- 0
    ratio <- rep(0,Niter_hmc)
    ratio_theory <- rep(0,Niter_hmc)
    time_HMC <- proc.time()
    i <- 1
    while(i < Niter_hmc)
    {
      r <- sample(1:L,size=1)
      Xstar <- HMC(U,grad_U,epsilon,r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i + 1
        Xsim[i,] <- Xstar[1:d]
        ratio[i] <- Xstar[d+1]
        ratio_theory[i] <- Xstar[d+2]
        Comp <- Comp + r + 2
      }
      if(i %% floor(Niter_hmc/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter_hmc,digits = 2)), "%",sep=""))
    }
    time_HMC <- proc.time() - time_HMC
    print(c(sum(ratio)/Niter_hmc,time_HMC))
    
    sampleRandom <- cbind(Xsim, -apply(Xsim,1,U))
    
    colnames(sampleRandom) <- NUTSnames
    
    return(list(sampleRandom=sampleRandom, L=L, Comp=Comp, epsilon=epsilon,Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  ###################### eHMC ######################
  eHMC <- function(Ind_trace,Niter,epsilon,theta)
  {
    #d <- 25
    #Niter_hmc <- floor(CompNUTS/(mean(Ind_trace)+2))
    Niter_hmc <- Niter
    print(Niter_hmc)
    Xsim <- matrix(0,nrow=Niter_hmc,ncol=d)
    Xsim[1,] <- theta
    
    Comp <- 0
    ratio <- rep(0,Niter_hmc)
    ratio_theory <- rep(0,Niter_hmc)
    time_HMC <- proc.time()
    i <- 1
    while(i < Niter_hmc)
    {
      #r <- sample(1:L,size=1)
      r <- sample(Ind_trace,size=1)
      Xstar <- HMC(U,grad_U,epsilon,r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i + 1
        Xsim[i,] <- Xstar[1:d]
        ratio[i] <- Xstar[d+1]
        ratio_theory[i] <- Xstar[d+2]
        Comp <- Comp + r + 2
      }
      if(i %% floor(Niter_hmc/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter_hmc,digits = 2)), "%",sep=""))
    }
    time_HMC <- proc.time() - time_HMC
    print(c(sum(ratio)/Niter_hmc,time_HMC))
    
    sampleRandom <- cbind(Xsim, -apply(Xsim,1,U))
    
    colnames(sampleRandom) <- NUTSnames
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  ###################### eHMCu ######################
  eHMCu <- function(Ind_trace,Niter,epsilon,theta)
  {
    #d <- 25
    #Niter_hmc <- floor(CompNUTS/(0.5*mean(Ind_trace)+2.5))
    Niter_hmc <- Niter
    print(Niter_hmc)
    Xsim <- matrix(0,nrow=Niter_hmc,ncol=d)
    Xsim[1,] <- theta
    
    Comp <- 0
    ratio <- rep(0,Niter_hmc)
    ratio_theory <- rep(0,Niter_hmc)
    time_HMC <- proc.time()
    i <- 1
    while(i < Niter_hmc)
    {
      L <- sample(Ind_trace,size=1)
      r <- sample(1:L,size=1)
      Xstar <- HMC(U,grad_U,epsilon,r,Xsim[i,])
      if(!is.na(Xstar[d+2]))
      {
        i <- i + 1
        Xsim[i,] <- Xstar[1:d]
        ratio[i] <- Xstar[d+1]
        ratio_theory[i] <- Xstar[d+2]
        Comp <- Comp + r + 2
      }
      if(i %% floor(Niter_hmc/10) == 0)
        print(paste("Iteration ",toString(100*round(i/Niter_hmc,digits = 2)), "%",sep=""))
    }
    time_HMC <- proc.time() - time_HMC
    print(c(sum(ratio)/Niter_hmc,time_HMC))
    
    sampleRandom <- cbind(Xsim, -apply(Xsim,1,U))
    
    colnames(sampleRandom) <- NUTSnames
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  
  #### Computing summary statistics
  fit0 <- ChooseL(epsilon0,init0,2e3)
  IndTrace0 <- fit0$Ind_trace
  thetaTrace0 <- fit0$Xsim[fit0$Niter,]
  rm(fit0)
  
  #hist(fit0$Ind_trace,breaks=50)
  #plot(fit0$Ind_trace,type="l")
  fit1 <- eHMCq(IndTrace0, CompNUTS, epsilon0, thetaTrace0)
  val <- fit1$sampleRandom[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_para <- sum(apply(val,1,function(xx) sum(xx^2)))/fit1$Comp
  
  val <- fit1$sampleRandom[,Index2]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/fit1$Comp
  
  val_ess <- ess(fit1$sampleRandom[,1:100])/fit1$Comp
  SummaryResult[2,] <- c(min(val_ess), mean(val_ess), median(val_ess), max(val_ess), ess(fit1$sampleRandom[,"lp__"])/fit1$Comp, ESJD_para, ESJD_lp, fit1$Comp, fit1$L, fit1$epsilon, fit1$Niter)
  rm(fit1)
  
  fit2 <- eHMC(IndTrace0, CompNUTS, epsilon0, thetaTrace0)
  val <- fit2$sampleRandom[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_para <- sum(apply(val,1,function(xx) sum(xx^2)))/fit2$Comp
  
  val <- fit2$sampleRandom[,Index2]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/fit2$Comp
  
  val_ess <- ess(fit2$sampleRandom[,1:100])/fit2$Comp
  SummaryResult[3,] <- c(min(val_ess), mean(val_ess), median(val_ess), max(val_ess), ess(fit2$sampleRandom[,"lp__"])/fit2$Comp, ESJD_para, ESJD_lp, fit2$Comp, mean(IndTrace0), fit2$epsilon, fit2$Niter)
  rm(fit2)
  
  fit3 <- eHMCu(IndTrace0, CompNUTS, epsilon0, thetaTrace0)
  val <- fit3$sampleRandom[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_para <- sum(apply(val,1,function(xx) sum(xx^2)))/fit3$Comp
  
  val <- fit3$sampleRandom[,Index2]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/fit3$Comp
  
  val_ess <- ess(fit3$sampleRandom[,1:100])/fit3$Comp
  SummaryResult[4,] <- c(min(val_ess), mean(val_ess), median(val_ess), max(val_ess), ess(fit3$sampleRandom[,"lp__"])/fit3$Comp, ESJD_para, ESJD_lp, fit3$Comp, 0.5*mean(IndTrace0)+0.5, fit3$epsilon, fit3$Niter)
  
  rm(fit3)
  return(list(SummaryResult=SummaryResult))
}


num_cores <- detectCores()

cl <- makeCluster(num_cores,type = "FORK")

### repeat 40 times
Result <- parLapply(cl, 1:40, function(zz) MixSampler(zz))

save(Result, file="Result.RData")
stopCluster(cl)

result1 <- Result[[1]]
result2 <- Result[[2]]

length(result1)
