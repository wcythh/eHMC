library(rstan)
library(mcmcse)
library(MASS)
library(transport)
library(parallel)
library(deSolve)
library(coda)

source("ModelSpecificFunctions/data.R")

#################### NUTS #############################
set.seed(1e7)
Index1 <- 1:1000
Index2 <- 1001:1003
Index3 <- 1004:1006
Index4 <- "lp__"

###################### NUTS ###################### 
rstan_options(auto_write = TRUE)
num_cores <- parallel::detectCores()
options(mc.cores = parallel::detectCores())
d <- T1+3
data = list(T1=T1,y=y)
iter0 <- 25e3
warmup0 <- 5e3
fit <- stan("SV.stan",data=data, chains=40, iter=iter0, warmup = warmup0,
            init = "random",save_dso = FALSE, verbose=TRUE,algorithm = "NUTS",
            control = list(adapt_engaged=TRUE,stepsize=0.01,adapt_delta=0.95,
                           metric="diag_e",max_treedepth=14))
source("ModelSpecificFunctions/UandGradU.R")
source("UniversalFunctions/Leapfrog.R")
source("UniversalFunctions/LongestBatch.R")
source("ModelSpecificFunctions/Transformation.R")
source("ModelSpecificFunctions/SummaryFunction.R")
source("UniversalFunctions/LearnL.R")
source("UniversalFunctions/eHMC.R")
source("UniversalFunctions/eHMCq.R")
source("UniversalFunctions/eHMCu.R")
adaptation_infoTotal = get_adaptation_info(fit)

for(k in 1:40)
{
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
  L_trace <- attr(fit@sim$samples[[k]],"sampler_params")$n_leapfrog__
  accept_trace <- attr(fit@sim$samples[[k]],"sampler_params")$accept_stat__
  CompNUTS <- sum(L_trace[-c(1:warmup0)])
  
  Xsamp_NUTS <- fit@sim$samples[[k]]
  Xsamp_NUTS <- Xsamp_NUTS[1:length(Xsamp_NUTS)]
  Xsamp_NUTS <- (t(do.call(rbind,Xsamp_NUTS)))[-c(1:warmup0),]
  NUTS_Summary <- list(Comp=CompNUTS, epsilon=epsilon0,
                       L_trace=L_trace[-c(1:warmup0)],
                       accept=mean(accept_trace[-c(1:warmup0)]),
                       statistics=SummaryFunction(Xsamp_NUTS),
                       M=M0,init=init0,feature_names=feature_names0)
  save(NUTS_Summary,file=paste("NUTS_Summary",toString(k),".RData",sep=""))
}

rm(fit)

MixSampler0 <- function(k)
{
  set.seed(k * 1e7)
  NUTS_Summary <- get(load(file=paste("NUTS_Summary",toString(k),".RData",sep="")))
  epsilon0 <- NUTS_Summary$epsilon
  M0 <- NUTS_Summary$M
  init0 <- NUTS_Summary$init
  feature_names0 <- NUTS_Summary$feature_names

  fit0 <- LearnL(epsilon0,M0,init0,d,2e3,feature_names0,U,grad_U,LongestBatch)
  LearnL_Summary <- list(L_emp=fit0$L_emp, accept=fit0$ratio/fit0$Niter)
  save(LearnL_Summary,file=paste("LearnL_Summary",toString(k),".RData",sep=""))
}
MixSampler1 <- function(k)
{
  set.seed(k * 1e7)
  NUTS_Summary <- get(load(file=paste("NUTS_Summary",toString(k),".RData",sep="")))
  LearnL_Summary <- get(load(file=paste("LearnL_Summary",toString(k),".RData",sep="")))
  epsilon0 <- NUTS_Summary$epsilon
  M0 <- NUTS_Summary$M
  init0 <- NUTS_Summary$init
  feature_names0 <- NUTS_Summary$feature_names
  
  fit1 <- eHMC( LearnL_Summary$L_emp,epsilon0,M0,init0,d,2e4,feature_names0,U,grad_U,Leapfrog)
  eHMC_Summary   <- list(Comp=fit1$Comp, epsilon=fit1$epsilon,
                         accept=fit1$ratio_empirical, time=fit1$time,
                         statistics=fit1$Xsummary)
  save(eHMC_Summary,file=paste("eHMC_Summary",toString(k),".RData",sep=""))
}

MixSampler2 <- function(k)
{
  set.seed(k * 1e7)
  NUTS_Summary <- get(load(file=paste("NUTS_Summary",toString(k),".RData",sep="")))
  LearnL_Summary <- get(load(file=paste("LearnL_Summary",toString(k),".RData",sep="")))
  epsilon0 <- NUTS_Summary$epsilon
  M0 <- NUTS_Summary$M
  init0 <- NUTS_Summary$init
  feature_names0 <- NUTS_Summary$feature_names
  
  fit2 <- eHMCq(LearnL_Summary$L_emp,epsilon0,M0,init0,d,2e4,feature_names0,U,grad_U,Leapfrog)
  eHMCq_Summary  <- list(Comp=fit2$Comp, epsilon=fit2$epsilon,
                         accept=fit2$ratio_empirical, time=fit2$time,
                         statistics=fit2$Xsummary)
  save(eHMCq_Summary,file=paste("eHMCq_Summary",toString(k),".RData",sep=""))
}

MixSampler3 <- function(k)
{
  set.seed(k * 1e7)
  NUTS_Summary <- get(load(file=paste("NUTS_Summary",toString(k),".RData",sep="")))
  LearnL_Summary <- get(load(file=paste("LearnL_Summary",toString(k),".RData",sep="")))
  epsilon0 <- NUTS_Summary$epsilon
  M0 <- NUTS_Summary$M
  init0 <- NUTS_Summary$init
  feature_names0 <- NUTS_Summary$feature_names
  
  fit3 <- eHMCu(LearnL_Summary$L_emp,epsilon0,M0,init0,d,2e4,feature_names0,U,grad_U,Leapfrog)
  eHMCu_Summary  <- list(Comp=fit3$Comp, epsilon=fit3$epsilon,
                         accept=fit3$ratio_empirical, time=fit3$time,
                         statistics=fit3$Xsummary)
  save(eHMCu_Summary,file=paste("eHMCu_Summary",toString(k),".RData",sep=""))
}


num_cores <- detectCores()

cl <- makeCluster(num_cores,type = "FORK")

Result0 <- parLapply(cl, 1:40, function(zz) MixSampler0(zz))
rm(Result0)
Result1 <- parLapply(cl, 1:40, function(zz) MixSampler1(zz))
rm(Result1)
Result2 <- parLapply(cl, 1:40, function(zz) MixSampler2(zz))
rm(Result2)
Result3 <- parLapply(cl, 1:40, function(zz) MixSampler3(zz))
rm(Result3)

stopCluster(cl)

