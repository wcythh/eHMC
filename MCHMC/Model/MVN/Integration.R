library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)
library(deSolve)
library(coda)


#################### NUTS #############################
set.seed(1e6)
rstan_options(auto_write = TRUE)
num_cores <- parallel::detectCores()
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

data = list(d=d,mu=mu,A=A)
iter0 <- 1e4
warmup0 <- 5e3
fit <- stan("MVN.stan",data=data, chains=1, iter=iter0, warmup = warmup0,
            init = "random",save_dso = FALSE, verbose=TRUE,algorithm = "NUTS",
            control = list(adapt_engaged=TRUE,stepsize=0.01,adapt_delta=0.8,
                           metric="unit_e",max_treedepth=14))
source("ModelSpecificFunctions/UandGradU.R")
source("UniversalFunctions/Leapfrog.R")
source("UniversalFunctions/LongestBatch.R")
source("ModelSpecificFunctions/Transformation.R")
source("ModelSpecificFunctions/SummaryFunction.R")
source("UniversalFunctions/LearnL.R")
source("UniversalFunctions/eHMC.R")
source("UniversalFunctions/eHMCq.R")
source("UniversalFunctions/eHMCu.R")
source("UniversalFunctions/Leapfrog_dense.R")
source("UniversalFunctions/CV_HMC.R")



adaptation_infoTotal = get_adaptation_info(fit)
MixSampler <- function(k)
{
  set.seed(k * 1e6)
  #adaptation_info <- adaptation_infoTotal[[k]]
  #M0 <- strsplit(adaptation_info[[1]],"\n")[[1]][4]
  #M0 <- strsplit(M0, "#")[[1]][2]
  #M0 <- as.vector(as.numeric(strsplit(M0, ",")[[1]]))
  #M0 <- 1/M0
  M0  <- rep(1,d)
  feature_names0 <- names(fit)
  stepsize_trace <- attr(fit@sim$samples[[k]],"sampler_params")$stepsize__
  epsilon0 <- stepsize_trace[iter0]
  init0 <- rep(0,d)
  for(l in 1:d)
  {
    init0[l] <- (fit@sim$samples[[k]])[[l]][warmup0+1]
  }
  init0 <- as.vector(init0)
  
  
  fit0 <- LearnL(epsilon0,M0,init0,d,2e3,feature_names0,U,grad_U,LongestBatch)
  fit1 <- eHMC( fit0$L_emp,epsilon0,M0,init0,d,0.98e4,feature_names0,U,grad_U,Leapfrog)
  fit2 <- eHMCq(fit0$L_emp,epsilon0,M0,init0,d,0.6e4,feature_names0,U,grad_U,Leapfrog)
  fit3 <- eHMCu(fit0$L_emp,epsilon0,M0,init0,d,2e4,feature_names0,U,grad_U,Leapfrog)
  
  fit4 <- CVHMC(0.1, fit0$L_emp,epsilon0,M0,init0,d,4.1e4,feature_names0,U,grad_U,Leapfrog_dense)
  
  L_trace <- attr(fit@sim$samples[[k]],"sampler_params")$n_leapfrog__
  accept_trace <- attr(fit@sim$samples[[k]],"sampler_params")$accept_stat__
  CompNUTS <- sum(L_trace[-c(1:warmup0)])
  
  Xsamp_NUTS <- fit@sim$samples[[k]]
  Xsamp_NUTS <- Xsamp_NUTS[1:length(Xsamp_NUTS)]
  Xsamp_NUTS <- (t(do.call(rbind,Xsamp_NUTS)))[-c(1:warmup0),]
  NUTS_Summary <- list(Comp=CompNUTS, epsilon=epsilon0,
                       L_trace=L_trace[-c(1:warmup0)],
                       accept=mean(accept_trace[-c(1:warmup0)]),
                       statistics=SummaryFunction(Xsamp_NUTS))
  LearnL_Summary <- list(L_emp=fit0$L_emp, accept=fit0$ratio/fit0$Niter)
  eHMC_Summary   <- list(Comp=fit1$Comp, epsilon=fit1$epsilon,
                         accept=fit1$ratio_empirical, time=fit1$time,
                         statistics=fit1$Xsummary)
  eHMCq_Summary  <- list(Comp=fit2$Comp, epsilon=fit2$epsilon,
                         accept=fit2$ratio_empirical, time=fit2$time,
                         statistics=fit2$Xsummary)
  eHMCu_Summary  <- list(Comp=fit3$Comp, epsilon=fit3$epsilon,
                         accept=fit3$ratio_empirical, time=fit3$time,
                         statistics=fit3$Xsummary)
  CVHMC_Summary  <- list(Comp=fit4$Comp, epsilon=fit4$epsilon,
                         accept=fit4$ratio_empirical, time=fit4$time,
                         statistics=fit4$Xsummary)
  
  NUTS_KS <- rep(0,d)
  eHMC_KS <- rep(0,d)
  eHMCq_KS <- rep(0,d)
  eHMCu_KS <- rep(0,d)
  CVHMC_KS <- rep(0,d)
  for(i in 1:d)
  {
    NUTS_KS[i] <- as.numeric(ks.test(Xsamp_NUTS[,i],"pnorm")$statistic)
    eHMC_KS[i] <- as.numeric(ks.test(fit1$Xsamp[,i], "pnorm")$statistic)
    eHMCq_KS[i] <- as.numeric(ks.test(fit2$Xsamp[,i], "pnorm")$statistic)
    eHMCu_KS[i] <- as.numeric(ks.test(fit3$Xsamp[,i], "pnorm")$statistic)
    CVHMC_KS[i] <- as.numeric(ks.test(fit4$Xsamp[,i], "pnorm")$statistic)
  }
  
  return(list(NUTS_Summary=NUTS_Summary, LearnL_Summary=LearnL_Summary,
              eHMC_Summary=eHMC_Summary, eHMCq_Summary=eHMCq_Summary, CVHMC_Summary=CVHMC_Summary,
              eHMCu_Summary=eHMCu_Summary, NUTS_KS=NUTS_KS, eHMC_KS=eHMC_KS,
              eHMCq_KS=eHMCq_KS, eHMCu_KS=eHMCu_KS, CVHMC_KS=CVHMC_KS))
}



num_cores <- detectCores()

cl <- makeCluster(num_cores,type = "FORK")

Result <- parLapply(cl, 1:40, function(zz) MixSampler(zz))

save(Result, file="Result.RData")
stopCluster(cl)

print(length(Result))

