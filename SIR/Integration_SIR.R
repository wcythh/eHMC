library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)
library(deSolve)

###################### data ###################### 
N_t <- 20
t0 <- c(7,14,21,28,35,42,49,56,63,70,77,84,91,98,105,112,119,126,133,140)
y0 <- c(10000,0,0,10000)
eta0 <- c(441,419,553,666,753,692,644,509,371,286,200,147,101,68,49,35,19,12,7,10)
B_hat <- c(6141.73,5801.72,9439.14,13742,11975,16387,15144,14705.6,8736.39,7553.29,6007.09,5979.96,3048.17,1566.77,1275.21,824.85,604.605,516.25,258.046,188.591)

###################### Integrated Samplers ######################
MixSampler <- function(zz)
{
  set.seed(zz*1e7)
  
  ###################### NUTS ###################### 
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores())
  d <- 4
  data <- list(N_t=N_t,t=t0,y0=y0,stoi_hat=eta0,B_hat=B_hat)
  init0 <- log(c(1.0120e+00,1.9979e-01,1.0546e+01,3.5613e-01))
  iter0 <- 25e3
  warmup0 <- 5e3
  fit <- stan("sir_log.stan",data=data,
              chains=1, iter=iter0, warmup = warmup0,
              init = init0,save_dso = FALSE,
              verbose=TRUE,
              control = list(adapt_engaged=TRUE,stepsize=0.001, 
                             metric="unit_e",adapt_delta=0.25),
              algorithm = "NUTS")
  
  sampleNUTS <- as.matrix(fit)
  result <- fit@sim$samples
  L_trace <- attr(result[[1]],"sampler_params")$n_leapfrog__
  stepsize_trace <- attr(result[[1]],"sampler_params")$stepsize__
  accept_trace <- attr(result[[1]],"sampler_params")$accept_stat__
  depth_trace <- attr(result[[1]],"sampler_params")$treedepth__
  epsilon0 <- stepsize_trace[iter0]
  CompNUTS <- sum((2*L_trace[-c(1:warmup0)]+1))
  
  ###################### functions ###################### 
  U <- function(theta)
  {
    val <- - log_prob(fit, theta)
    return(val)
  }
  
  grad_U <- function(theta)
  {
    val <- - grad_log_prob(fit, theta)[1:d]
    return(val)
  }
  
  ###################### generating longest batches ######################
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
  
  ###################### classical HMC sampler ######################
  HMC <- function(U,grad_U,epsilon,L,current_q)
  {
    q = current_q
    p = rnorm(length(q),0,1)
    current_p = p
    p = p - 0.5*epsilon * (grad_U(q))
    for(l in 1:L)
    {
      q = q + epsilon * p
      if(l != L)
        p = p - epsilon * grad_U(q)
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
  
  ###################### SIR dynamics ######################
  SIR <- function(t,y,theta)
  {
    dydt <- rep(0,4)
    dydt[1] <- -theta[1]*y[4]/(y[4]+theta[2]) *y[1]
    dydt[2] <- theta[1]*y[4]/(y[4]+theta[2])*y[1] - theta[3]*y[2]
    dydt[3] <- theta[3]*y[2]
    dydt[4] <- theta[4]*y[2] - theta[5]*y[4]
    return(list(dydt))
  }
  
  ###################### ODE solver ######################
  Integrate_ode45 <- function(theta)
  {
    inits <- y0
    times <- c(0,t0)
    kappa0 <- 1e6
    theta0 <- c(theta[1],kappa0,theta[2],theta[3],theta[4])
    out <- ode(inits, times, SIR, parms=theta0, method="ode45")
    out <- as.matrix(out)
    out <- out[,-1]
    return(out)
  }
  ###################### generating simulated observations ######################
  generated_quantity <- function(phi)
  {
    theta <- exp(phi)
    y_hat <- Integrate_ode45(theta)
    y_hat <- y_hat[-1,]
    return(as.vector(y_hat))
  }
  
  ###################### generating empirical distribution (Algorithm 2) ######################
  ChooseL <- function(epsilon, theta, Niter)
  {
    d <- 4
    Niter <- 1e3
    Xsim <- matrix(0,nrow=Niter,ncol=d)
    Xsim[1,] <- theta
    print(Xsim[1,])
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
        Comp_Random <- Comp_Random + 2*L_r+2
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
    d <- 4
    L <- as.numeric(quantile(Ind_trace, 0.95))
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
    
    sampleRandom <- cbind(Xsim, exp(Xsim), t(apply(Xsim,1,generated_quantity)),
                          -apply(Xsim,1,U))
    
    colnames(sampleRandom) <- colnames(sampleNUTS)
    
    return(list(sampleRandom=sampleRandom, L=L, Comp=Comp, epsilon=epsilon,Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  
  ###################### eHMC ######################
  eHMC <- function(Ind_trace,Niter,epsilon,theta)
  {
    d <- 4
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
    
    sampleRandom <- cbind(Xsim, exp(Xsim), t(apply(Xsim,1,generated_quantity)),
                          -apply(Xsim,1,U))
    
    colnames(sampleRandom) <- colnames(sampleNUTS)
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  
  ###################### eHMCu ######################
  eHMCu <- function(Ind_trace,Niter,epsilon,theta)
  {
    d <- 4
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
    
    sampleRandom <- cbind(Xsim, exp(Xsim), t(apply(Xsim,1,generated_quantity)),
                          -apply(Xsim,1,U))
    
    colnames(sampleRandom) <- colnames(sampleNUTS)
    
    return(list(sampleRandom=sampleRandom, Comp=Comp, epsilon=epsilon, Niter=Niter_hmc,
                ratio_empirical=ratio, ratio_theory=ratio_theory,
                time=time_HMC))
  }
  
  fit0 <- ChooseL(epsilon0,init0,2e3)
  #hist(fit0$Ind_trace,breaks=50)
  #plot(fit0$Ind_trace,type="l")
  fit1 <- eHMCq(fit0$Ind_trace, 2e4, epsilon0, fit0$Xsim[fit0$Niter,])
  fit2 <- eHMC(fit0$Ind_trace, 2e4, epsilon0, fit0$Xsim[fit0$Niter,])
  fit3 <- eHMCu(fit0$Ind_trace, 2e4, epsilon0, fit0$Xsim[fit0$Niter,])

  return(list(fitNUTS=fit, fit0=fit0,fit1=fit1, fit2=fit2, fit3=fit3))
}


num_cores <- detectCores()

cl <- makeCluster(num_cores,type = "FORK")

#### Repeat 40 times
Result <- parLapply(cl, 1:40, function(zz) MixSampler(zz))

save(Result, file="Result.RData")
stopCluster(cl)

result1 <- Result[[1]]
result2 <- Result[[2]]

length(result1)
