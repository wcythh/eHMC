library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)
library(deSolve)
library(coda)

source("ModelSpecificFunctions/data.R")

#################### NUTS #############################
set.seed(1e6)
rstan_options(auto_write = TRUE)
num_cores <- parallel::detectCores()
options(mc.cores = parallel::detectCores())
d <- 4
data <- list(N_t=N_t,t=t0,y0=y0,stoi_hat=eta0,B_hat=B_hat)
iter0 <- 2e3
warmup0 <- 1e3
fit <- stan("sir_log.stan",data=data, chains=1, iter=iter0, warmup = warmup0,
            init = "random",save_dso = FALSE, verbose=TRUE,algorithm = "NUTS",
            control = list(adapt_engaged=TRUE,stepsize=0.01,adapt_delta=0.8,
                           metric="diag_e",max_treedepth=14))
#Benchmark <- fit@sim$samples[[1]]
#Benchmark <- Benchmark[1:length(Benchmark)]
#Benchmark <- (t(do.call(rbind,Benchmark)))[-c(1:warmup0),]
#save(Benchmark, file="Benchmark.RData")
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

Benchmark <- get(load(file="Benchmark.RData"))
adaptation_infoTotal = get_adaptation_info(fit)
MixSampler <- function(k)
{
  set.seed(k * 1e6)
  adaptation_info <- adaptation_infoTotal[[k]]
  M0 <- strsplit(adaptation_info[[1]],"\n")[[1]][4]
  M0 <- strsplit(M0, "#")[[1]][2]
  M0 <- as.vector(as.numeric(strsplit(M0, ",")[[1]]))
  M0 <- 1/M0
  #M0  <- rep(1,d)
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
  fit1 <- eHMC( fit0$L_emp,epsilon0,M0,init0,d,13e3,feature_names0,U,grad_U,Leapfrog)
  fit2 <- eHMCq(fit0$L_emp,epsilon0,M0,init0,d,13e3,feature_names0,U,grad_U,Leapfrog)
  fit3 <- eHMCu(fit0$L_emp,epsilon0,M0,init0,d,24.5e3,feature_names0,U,grad_U,Leapfrog)
  
  fit4 <- CVHMC(eta=0.1, fit0$L_emp,epsilon0,M0,init0,d,45e3,feature_names0,U,grad_U,Leapfrog_dense)
  
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
  
  d0 <- 89
  NUTS_KS <- rep(0,d0)
  eHMC_KS <- rep(0,d0)
  eHMCq_KS <- rep(0,d0)
  eHMCu_KS <- rep(0,d0)
  CVHMC_KS <- rep(0,d0)
  for(i in 1:d0)
  {
    valTemp <- Benchmark[,i]
    NUTS_KS[i] <- as.numeric(ks.test(Xsamp_NUTS[,i],valTemp)$statistic)
    eHMC_KS[i] <- as.numeric(ks.test(fit1$Xsamp[,i], valTemp)$statistic)
    eHMCq_KS[i] <- as.numeric(ks.test(fit2$Xsamp[,i], valTemp)$statistic)
    eHMCu_KS[i] <- as.numeric(ks.test(fit3$Xsamp[,i], valTemp)$statistic)
    CVHMC_KS[i] <- as.numeric(ks.test(fit4$Xsamp[,i], valTemp)$statistic)
  }
  
  #return(list(NUTS_Summary=NUTS_Summary, LearnL_Summary=LearnL_Summary,
  #            eHMC_Summary=eHMC_Summary, eHMCq_Summary=eHMCq_Summary,
  #            eHMCu_Summary=eHMCu_Summary))
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

