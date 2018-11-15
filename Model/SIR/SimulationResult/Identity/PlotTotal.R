#metric: ESS of beta, ESS of gamma, ESS of xi, ESS of phi       -- ess of mcmcse
#        ESS of beta, ESS of gamma, ESS of xi, ESS of phi       -- effectiveSize of coda
#        ESS of lp (mcmcse), ESS of lp (coda)                   
#        Normalized ESJD_theta, Normalized ESJD_lp
#        Computation Cost, Tuned epsilon, Expected L, empirical accept probability
Ind <- c(seq(60,95,by=5))
IndFile <- c("60", "65",
             "70", "75",
             "80", "85",
             "90", "95")
library(mcmcse)
################################ Data Processing #################################
MetricNUTS    <- list()
MetriceHMC    <- list()
MetriceHMCq   <- list()
MetriceHMCu   <- list()
for(k in 1:8)
{
  MetricNUTS[[k]] <- matrix(0,nrow=40,ncol=16)
  MetriceHMC[[k]] <- matrix(0,nrow=40,ncol=16)
  MetriceHMCq[[k]] <- matrix(0,nrow=40,ncol=16)
  MetriceHMCu[[k]] <- matrix(0,nrow=40,ncol=16)
  FitTemp <- get(load(file=paste("Identity/Result/",IndFile[k],"/Result.RData",sep="")))
  for(m in 1:40)
  {
    val <- FitTemp[[m]]
    valTemp <- val$NUTS_Summary
    MetricNUTS[[k]][m,] <- c((valTemp$statistics$ess_by_mcmcse[5])/valTemp$Comp,
                             (valTemp$statistics$ess_by_mcmcse[6])/valTemp$Comp,
                             (valTemp$statistics$ess_by_mcmcse[7])/valTemp$Comp,
                             (valTemp$statistics$ess_by_mcmcse[8])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[5])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[6])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[7])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[8])/valTemp$Comp,
                             valTemp$statistics$ess_by_mcmcse[89]/valTemp$Comp,
                             valTemp$statistics$ess_by_coda[89]/valTemp$Comp,
                             valTemp$statistics$ESJD_para/valTemp$Comp,
                             valTemp$statistics$ESJD_lp/valTemp$Comp,
                             valTemp$Comp,val$NUTS_Summary$epsilon,
                             mean(valTemp$L_trace),
                             valTemp$accept)
    
    valTemp <- val$eHMC_Summary
    MetriceHMC[[k]][m,] <- c((valTemp$statistics$ess_by_mcmcse[5])/valTemp$Comp,
                             (valTemp$statistics$ess_by_mcmcse[6])/valTemp$Comp,
                             (valTemp$statistics$ess_by_mcmcse[7])/valTemp$Comp,
                             (valTemp$statistics$ess_by_mcmcse[8])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[5])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[6])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[7])/valTemp$Comp,
                             (valTemp$statistics$ess_by_coda[8])/valTemp$Comp,
                             valTemp$statistics$ess_by_mcmcse[89]/valTemp$Comp,
                             valTemp$statistics$ess_by_coda[89]/valTemp$Comp,
                             valTemp$statistics$ESJD_para/valTemp$Comp,
                             valTemp$statistics$ESJD_lp/valTemp$Comp,
                             valTemp$Comp,val$NUTS_Summary$epsilon,
                             mean(val$LearnL_Summary$L_emp),
                             valTemp$accept)
    valTemp <- val$eHMCq_Summary
    MetriceHMCq[[k]][m,] <- c((valTemp$statistics$ess_by_mcmcse[5])/valTemp$Comp,
                              (valTemp$statistics$ess_by_mcmcse[6])/valTemp$Comp,
                              (valTemp$statistics$ess_by_mcmcse[7])/valTemp$Comp,
                              (valTemp$statistics$ess_by_mcmcse[8])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[5])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[6])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[7])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[8])/valTemp$Comp,
                              valTemp$statistics$ess_by_mcmcse[89]/valTemp$Comp,
                              valTemp$statistics$ess_by_coda[89]/valTemp$Comp,
                              valTemp$statistics$ESJD_para/valTemp$Comp,
                              valTemp$statistics$ESJD_lp/valTemp$Comp,
                              valTemp$Comp,val$NUTS_Summary$epsilon,
                              quantile(val$LearnL_Summary$L_emp,0.95),
                              valTemp$accept)
    valTemp <- val$eHMCu_Summary
    MetriceHMCu[[k]][m,] <- c((valTemp$statistics$ess_by_mcmcse[5])/valTemp$Comp,
                              (valTemp$statistics$ess_by_mcmcse[6])/valTemp$Comp,
                              (valTemp$statistics$ess_by_mcmcse[7])/valTemp$Comp,
                              (valTemp$statistics$ess_by_mcmcse[8])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[5])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[6])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[7])/valTemp$Comp,
                              (valTemp$statistics$ess_by_coda[8])/valTemp$Comp,
                              valTemp$statistics$ess_by_mcmcse[89]/valTemp$Comp,
                              valTemp$statistics$ess_by_coda[89]/valTemp$Comp,
                              valTemp$statistics$ESJD_para/valTemp$Comp,
                              valTemp$statistics$ESJD_lp/valTemp$Comp,
                              valTemp$Comp,val$NUTS_Summary$epsilon,
                              mean(val$LearnL_Summary$L_emp)/2 + 0.5,
                              valTemp$accept)
  }
  print(k)
}

k <- 1
IndTemp <- 1
min0 <- min(c(min(MetricNUTS[[k]][,IndTemp]), min(MetriceHMC[[k]][,IndTemp]),
              min(MetriceHMCq[[k]][,IndTemp]),min(MetriceHMCu[[k]][,IndTemp])))
max0 <- max(c(max(MetricNUTS[[k]][,IndTemp]), max(MetriceHMC[[k]][,IndTemp]),
              max(MetriceHMCq[[k]][,IndTemp]),    max(MetriceHMCu[[k]][,IndTemp])))
plot(MetricNUTS[[k]][,IndTemp],type="o",pch=20,lwd=3,ylim=c(min0,max0))
lines(MetriceHMC[[k]][,IndTemp],type="o",pch=20,col="red",lwd=3)
lines(MetriceHMCq[[k]][,IndTemp],type="o",pch=20,col="blue",lwd=3)
lines(MetriceHMCu[[k]][,IndTemp],type="o",pch=20,col="green",lwd=3)

n <- 1
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability:" ~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~beta) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.03))


n <- 2
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~gamma) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.025))



n <- 3
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~xi) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.03))

n <- 4
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~phi) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.03))


n <- 5
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~beta) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.025))


n <- 6
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~gamma) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.025))

n <- 7
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~xi) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.025))


n <- 8
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESS per gradient") + 
  ggtitle("SIR: ESS of" ~phi) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.025))


n <- 11
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(Ind,apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(Ind,apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(Ind,apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(Ind,apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("ESJD per gradient") + 
  ggtitle("SIR: ESJD of" ~theta) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.025))

n <- 14
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(seq(0.6,0.95,by=0.05),apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(seq(0.6,0.95,by=0.05),apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(seq(0.6,0.95,by=0.05),apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(seq(0.6,0.95,by=0.05),apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab(expression(epsilon)) + 
  ggtitle("SIR (diagonal):" ~epsilon) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) +
  ylim(c(0,0.005))



n <- 15
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp] * MetricNUTS[[k]][,14]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp] * MetriceHMC[[k]][,14]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp]* MetriceHMCq[[k]][,14]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp]* MetriceHMCu[[k]][,14]
}

min0 <- min(c(min(ESSNUTS), min(ESS1), min(ESS2), min(ESS3)))
max0 <- max(c(max(ESSNUTS), max(ESS1), max(ESS2), max(ESS3)))
plot(cbind(seq(0.6,0.95,by=0.05),apply(ESSNUTS,1,median)),ylim=c(0,max0),
     type="o",pch=20,lwd=2,col="black",ylab="",xlab="",main="Min ESS")
lines(cbind(seq(0.6,0.95,by=0.05),apply(ESS1,1,median)),type="o",pch=20,lwd=2,col="red")
lines(cbind(seq(0.6,0.95,by=0.05),apply(ESS2,1,median)),type="o",pch=20,lwd=2,col="blue")
lines(cbind(seq(0.6,0.95,by=0.05),apply(ESS3,1,median)),type="o",pch=20,lwd=2,col="green")


ESSTransformed <- NULL
for(k in 1:8)
{
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESSNUTS[k,], 0))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS1[k,], 1))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS2[k,], 2))
  ESSTransformed <- rbind(ESSTransformed, cbind(Ind[k]/100, ESS3[k,], 3))
}

ESSTransformed <- data.frame(ESSTransformed)
colnames(ESSTransformed) <-  c("x", "y", "Sampler")
IndexSampler <- which(ESSTransformed[,"Sampler"] == 0)
ESSTransformed[IndexSampler, "Sampler"] <- "NUTS"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 1)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMC"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 2)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCq"
IndexSampler <- which(ESSTransformed[,"Sampler"] == 3)
ESSTransformed[IndexSampler, "Sampler"] <- "eHMCu"

ESSSummary <- rbind(cbind(Ind/100, apply(ESSNUTS,1,median), 0),
                    cbind(Ind/100, apply(ESS1,1,median), 1),
                    cbind(Ind/100, apply(ESS2,1,median), 2),
                    cbind(Ind/100, apply(ESS3,1,median), 3))
ESSSummary <- data.frame(ESSSummary)
colnames(ESSSummary) <- c("x", "y", "Sampler")
IndexSampler <- which(ESSSummary[,"Sampler"] == 0)
ESSSummary[IndexSampler,"Sampler"] <- "NUTS"
IndexSampler <- which(ESSSummary[,"Sampler"] == 1)
ESSSummary[IndexSampler,"Sampler"] <- "eHMC"
IndexSampler <- which(ESSSummary[,"Sampler"] == 2)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCq"
IndexSampler <- which(ESSSummary[,"Sampler"] == 3)
ESSSummary[IndexSampler,"Sampler"] <- "eHMCu"

neworder <- c("NUTS","eHMC","eHMCq","eHMCu")
library(plyr)  ## or dplyr (transform -> mutate)
library(ggplot2)
ESSTransformed <- arrange(transform(ESSTransformed,
                                    Sampler=factor(Sampler,levels=neworder)),Sampler)

ggplot(data=ESSTransformed,mapping=aes(x=x,y=y)) + 
  geom_point(alpha=1,size=0.4) +
  geom_line(data=ESSSummary, mapping=aes(x=x, y=y), color="red", size=1) +
  facet_grid(cols = vars(factor(Sampler))) + xlab("Desired accept probability: "~delta) + 
  ylab("Integration Time") + 
  ggtitle("SIR: Integration Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.31))
