library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)
library(deSolve)

###################### generating data ######################
set.seed(123456)
T1 <- 1000
kappa0 <- 0.65
sigma0 <- 0.15
phi0 <- 0.98
x0 <- rep(0,T1)
y <- rep(0,T1)
x0[1] <- rnorm(1,mean = 0, sd = (sigma0/sqrt(1-phi0^2)))
for(t in 2:T1)
{
  x0[t] <- phi0 * x0[t-1] + rnorm(1,mean=0,sd=sigma0)
}
y <- kappa0*exp(0.5*x0)*rnorm(T1)

Index1 <- 1:1000
Index2 <- 1001:1003
Index3 <- 1004:1006
Index4 <- "lp__"

###################### Integrated Samplers ######################
MixSampler <- function(zz)
{
  set.seed(zz*1e6)
  
  SummaryResult <- matrix(0,nrow=4,ncol=19)
  
  ###################### NUTS ###################### 
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  d <- T1+3
  data = list(T1=T1,y=y)
  init0 <- c(x0, log((1+phi0)/(1-phi0)), log(kappa0), 2*log(sigma0))
  iter0 <- 25e3
  warmup0 <- 5e3
  fit <- stan("SV.stan",data=data, chains=1, iter=iter0, warmup = warmup0,
              init = init0, save_dso = TRUE, verbose=FALSE,
              control = list(adapt_engaged=TRUE,stepsize=0.01,adapt_delta=0.85,
                             metric="unit_e",max_treedepth=14),
              algorithm = "NUTS")
  
  sampleNUTS <- as.matrix(fit)
  result <- fit@sim$samples
  L_trace <- attr(result[[1]],"sampler_params")$n_leapfrog__
  stepsize_trace <- attr(result[[1]],"sampler_params")$stepsize__
  accept_trace <- attr(result[[1]],"sampler_params")$accept_stat__
  depth_trace <- attr(result[[1]],"sampler_params")$treedepth__
  epsilon0 <- stepsize_trace[iter0]
  CompNUTS <- sum((2*L_trace[-c(1:warmup0)]+1))
  
  ###################### Computing statistics of NUTS ######################
  val <- sampleNUTS[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_x <- sum(apply(val,1,function(xx) sum(xx^2)))/CompNUTS
  
  val <- sampleNUTS[,Index2]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_log <- sum(apply(val,1,function(xx) sum(xx^2)))/CompNUTS
  
  val <- sampleNUTS[,Index3]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_theta <- sum(apply(val,1,function(xx) sum(xx^2)))/CompNUTS
  
  val <- sampleNUTS[,Index4]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/CompNUTS
  
  valESS <- ess(sampleNUTS[,Index1])/CompNUTS
  valX <- c(min(valESS),mean(valESS),median(valESS),max(valESS))
  valLog <- ess(sampleNUTS[,Index2])/CompNUTS
  valTheta <- ess(sampleNUTS[,Index3])/CompNUTS
  valLP <- ess(sampleNUTS[,Index4])/CompNUTS
  
  SummaryResult[1,] <- c(valX, valLog, valTheta, valLP,
  ESJD_x, ESJD_log, ESJD_theta, ESJD_lp,
  CompNUTS, mean(L_trace[-c(1:warmup0)]),
  epsilon0, nrow(sampleNUTS))
  
  NUTSnames <- colnames(sampleNUTS)
  rm(fit)
  rm(result)
  rm(sampleNUTS)
  
  ###################### functions ######################
  ## energy
  U <- function(theta)
  {
      alpha <- theta[(T1+1)]
      beta <- theta[(T1+2)]
      gamma <- theta[(T1+3)]
      x <- theta[1:T1]
      phi <- (exp(alpha)-1)/(exp(alpha)+1)
      kappa <- exp(beta)
      sigma2 <- exp(gamma)
      val <- T1*beta + 0.5*sum(x)+sum((y^2/exp(x)))/(2*kappa^2) - 20.5*alpha + 22.5*log(exp(alpha)+1) +
      (0.5*T1 + 5)*gamma + (x[1]^2)*2*exp(alpha)/(exp(gamma) * ((exp(alpha)+1)^2)) +
      sum((x[2:T1] - phi*x[1:(T1-1)])^2)/(2*exp(gamma)) + 0.25/exp(gamma)
      return(val)
  }
  ## gradient of energy
  grad_U <- function(theta)
  {
      alpha <- theta[(T1+1)]
      beta <- theta[(T1+2)]
      gamma <- theta[(T1+3)]
      x <- theta[1:T1]
      phi <- (exp(alpha)-1)/(exp(alpha)+1)
      kappa <- exp(beta)
      sigma2 <- exp(gamma)
      p1 <- 0.5-(y[1]^2)/(2*exp(x[1])*kappa^2) + 4*x[1]*exp(alpha)/(exp(gamma)*(exp(alpha)+1)^2) +
      phi*(phi*x[1]-x[2])/exp(gamma)
      p2 <- 0.5-(y[2:(T1-1)]^2)/(2*exp(x[2:(T1-1)])*kappa^2) + (x[2:(T1-1)]-phi*x[1:(T1-2)])/exp(gamma)+
      phi*(phi*x[2:(T1-1)] -x[3:T1])/exp(gamma)
      p3 <- 0.5-(y[T1]^2)/(2*exp(x[T1])*kappa^2) + (x[T1]-phi*x[(T1-1)])/exp(gamma)
      p4 <- -20.5 + 22.5*exp(alpha)/(exp(alpha)+1) + 2*(x[1]^2)*exp(alpha)*(1-exp(alpha))/(exp(gamma)*(1+exp(alpha))^3)+
      sum((phi*x[1:(T1-1)] - x[2:T1])*x[1:(T1-1)])*2*exp(alpha)/(exp(gamma)*(exp(alpha)+1)^2)
      p5 <- - sum(y^2/exp(x))/(kappa^2) + T1
      p6 <- 0.5*T1 + 5 - 2*(x[1]^2)*exp(alpha)/(exp(gamma)*(exp(alpha)+1)^2) -
      sum((x[2:T1]-phi*x[1:(T1-1)])^2)/(2*exp(gamma)) - 0.25/exp(gamma)
      return(c(p1,p2,p3,p4,p5,p6))
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
  
  ###################### Classical HMC Sampler ######################
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
    p = p - 0.5*epsilon * grad_U(q)
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
  
  ###################### Generating empirical distribution (Algorithm 2) ######################
  ChooseL <- function(epsilon, theta, Niter)
  {
    Xsim <- matrix(0,nrow=Niter,ncol=d)
    Xsim[1,] <- theta
    L <- 50
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
  
  ############################## eHMCq Sampler ###################################
  eHMCq <- function(Ind_trace,Niter,epsilon,theta)
  {
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
    
    sampleRandom <- cbind(Xsim, (exp(Xsim[,1001])-1)/(exp(Xsim[,1001])+1),
                          exp(Xsim[,1002]), exp(0.5*Xsim[,1003]) ,-apply(Xsim,1,U))
    
    colnames(sampleRandom) <- NUTSnames
    
    return(list(sampleRandom=sampleRandom, L=L, Comp=Comp, epsilon=epsilon,Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  ###################### eHMC ######################
  eHMC <- function(Ind_trace,Niter,epsilon,theta)
  {
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
    
    sampleRandom <- cbind(Xsim, (exp(Xsim[,1001])-1)/(exp(Xsim[,1001])+1),
                          exp(Xsim[,1002]), exp(0.5*Xsim[,1003]) ,-apply(Xsim,1,U))
    
    colnames(sampleRandom) <- NUTSnames
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  ###################### eHMCu ######################
  eHMCu <- function(Ind_trace,Niter,epsilon,theta)
  {
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
    
    sampleRandom <- cbind(Xsim, (exp(Xsim[,1001])-1)/(exp(Xsim[,1001])+1),
                          exp(Xsim[,1002]), exp(0.5*Xsim[,1003]) ,-apply(Xsim,1,U))
    
    colnames(sampleRandom) <- NUTSnames
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  
  #### Generating emprical distribution
  fit0 <- ChooseL(epsilon0, init0, 2e3)
  
  IndTrace0 <- fit0$Ind_trace
  thetaTrace0 <- fit0$Xsim[fit0$Niter,]
  rm(fit0)
  
  #### run eHMCq
  fit1 <- eHMCq(IndTrace0, 2e4, epsilon0, thetaTrace0)
  val <- fit1$sampleRandom[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_x <- sum(apply(val,1,function(xx) sum(xx^2)))/fit1$Comp
  
  val <- fit1$sampleRandom[,Index2]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_log <- sum(apply(val,1,function(xx) sum(xx^2)))/fit1$Comp
  
  val <- fit1$sampleRandom[,Index3]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_theta <- sum(apply(val,1,function(xx) sum(xx^2)))/fit1$Comp
  
  val <- fit1$sampleRandom[,Index4]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/fit1$Comp
  
  valESS <- ess(fit1$sampleRandom[,Index1])/fit1$Comp
  valX <- c(min(valESS),mean(valESS),median(valESS),max(valESS))
  valLog <- ess(fit1$sampleRandom[,Index2])/fit1$Comp
  valTheta <- ess(fit1$sampleRandom[,Index3])/fit1$Comp
  valLP <- ess(fit1$sampleRandom[,Index4])/fit1$Comp
  
  SummaryResult[2,] <- c(valX, valLog, valTheta, valLP,
  ESJD_x, ESJD_log, ESJD_theta, ESJD_lp,
  fit1$Comp, fit1$L, fit1$epsilon, fit1$Niter)
  
  rm(fit1)
  
  #### run eHMC
  fit2 <- eHMC(IndTrace0, 2e4, epsilon0, thetaTrace0)
  val <- fit2$sampleRandom[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_x <- sum(apply(val,1,function(xx) sum(xx^2)))/fit2$Comp
  
  val <- fit2$sampleRandom[,Index2]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_log <- sum(apply(val,1,function(xx) sum(xx^2)))/fit2$Comp
  
  val <- fit2$sampleRandom[,Index3]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_theta <- sum(apply(val,1,function(xx) sum(xx^2)))/fit2$Comp
  
  val <- fit2$sampleRandom[,Index4]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/fit2$Comp
  
  valESS <- ess(fit2$sampleRandom[,Index1])/fit2$Comp
  valX <- c(min(valESS),mean(valESS),median(valESS),max(valESS))
  valLog <- ess(fit2$sampleRandom[,Index2])/fit2$Comp
  valTheta <- ess(fit2$sampleRandom[,Index3])/fit2$Comp
  valLP <- ess(fit2$sampleRandom[,Index4])/fit2$Comp
  
  SummaryResult[3,] <- c(valX, valLog, valTheta, valLP,
  ESJD_x, ESJD_log, ESJD_theta, ESJD_lp,
  fit2$Comp, mean(IndTrace0), fit2$epsilon, fit2$Niter)
  
  rm(fit2)
  
  ### run eHMCu
  fit3 <- eHMCu(IndTrace0, 2e4, epsilon0, thetaTrace0)
  
  val <- fit3$sampleRandom[,Index1]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_x <- sum(apply(val,1,function(xx) sum(xx^2)))/fit3$Comp
  
  val <- fit3$sampleRandom[,Index2]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_log <- sum(apply(val,1,function(xx) sum(xx^2)))/fit3$Comp
  
  val <- fit3$sampleRandom[,Index3]
  val <- val[-1,] - val[-nrow(val),]
  ESJD_theta <- sum(apply(val,1,function(xx) sum(xx^2)))/fit3$Comp
  
  val <- fit3$sampleRandom[,Index4]
  val <- val[-1] - val[-length(val)]
  ESJD_lp <- sum(val^2)/fit3$Comp
  
  valESS <- ess(fit3$sampleRandom[,Index1])/fit3$Comp
  valX <- c(min(valESS),mean(valESS),median(valESS),max(valESS))
  valLog <- ess(fit3$sampleRandom[,Index2])/fit3$Comp
  valTheta <- ess(fit3$sampleRandom[,Index3])/fit3$Comp
  valLP <- ess(fit3$sampleRandom[,Index4])/fit3$Comp
  
  SummaryResult[4,] <- c(valX, valLog, valTheta, valLP,
                        ESJD_x, ESJD_log, ESJD_theta, ESJD_lp,
                        fit3$Comp, 0.5*mean(IndTrace0)+0.5,
                        fit3$epsilon, fit3$Niter)
  rm(fit3)
  return(list(SummaryResult=SummaryResult))
}


num_cores <- detectCores()

cl <- makeCluster(num_cores,type = "FORK")

## Repeat programe 40 times
Result <- parLapply(cl, 1:40, function(zz) MixSampler(zz))

save(Result, file="Result.RData")
stopCluster(cl)

result1 <- Result[[1]]
result2 <- Result[[2]]

length(result1)
