#Metric: MinESS of x, ESS of phi, ESS of kappa, ESS of sigma  -- ess of mcmcse
#        MinESS of x, ESS of phi, ESS of kappa, ESS of sigma  -- effectiveSize of coda
#        ESS of lp (mcmcse), ESS of lp (coda)
#        Normalized ESJD of x, Normalized ESJD of hyperparameters, Normalized ESJD of lp
#        Computation Cost
#        Tuned epsilon 
#        Expected L
#        empirical accept probability
        
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
  MetricNUTS[[k]] <- matrix(0,nrow=40,ncol=17)
  MetriceHMC[[k]] <- matrix(0,nrow=40,ncol=17)
  MetriceHMCq[[k]] <- matrix(0,nrow=40,ncol=17)
  MetriceHMCu[[k]] <- matrix(0,nrow=40,ncol=17)
  FitTemp <- get(load(file=paste("Identity/Result/",IndFile[k],"/Result.RData",sep="")))
  for(m in 1:40)
  {
    val <- FitTemp[[m]]
    MetricNUTS[[k]][m,] <- c(min(val$NUTS_Summary$statistics$ess_by_mcmcse[1:1000])/val$NUTS_Summary$Comp,
                             (val$NUTS_Summary$statistics$ess_by_mcmcse[1004])/val$NUTS_Summary$Comp,
                             (val$NUTS_Summary$statistics$ess_by_mcmcse[1005])/val$NUTS_Summary$Comp,
                             (val$NUTS_Summary$statistics$ess_by_mcmcse[1006])/val$NUTS_Summary$Comp,
                             min(val$NUTS_Summary$statistics$ess_by_coda[1:1000])/val$NUTS_Summary$Comp,
                             (val$NUTS_Summary$statistics$ess_by_coda[1004])/val$NUTS_Summary$Comp,
                             (val$NUTS_Summary$statistics$ess_by_coda[1005])/val$NUTS_Summary$Comp,
                             (val$NUTS_Summary$statistics$ess_by_coda[1006])/val$NUTS_Summary$Comp,
                             val$NUTS_Summary$statistics$ess_by_mcmcse[1007]/val$NUTS_Summary$Comp,
                             val$NUTS_Summary$statistics$ess_by_coda[1007]/val$NUTS_Summary$Comp,
                             val$NUTS_Summary$statistics$ESJD_x/val$NUTS_Summary$Comp,
                             val$NUTS_Summary$statistics$ESJD_para/val$NUTS_Summary$Comp,
                             val$NUTS_Summary$statistics$ESJD_lp/val$NUTS_Summary$Comp,
                             val$NUTS_Summary$Comp,val$NUTS_Summary$epsilon,
                             mean(val$NUTS_Summary$L_trace),
                             val$NUTS_Summary$accept)
    
    MetriceHMC[[k]][m,] <- c(min(val$eHMC_Summary$statistics$ess_by_mcmcse[1:1000])/val$eHMC_Summary$Comp,
                             (val$eHMC_Summary$statistics$ess_by_mcmcse[1004])/val$eHMC_Summary$Comp,
                             (val$eHMC_Summary$statistics$ess_by_mcmcse[1005])/val$eHMC_Summary$Comp,
                             (val$eHMC_Summary$statistics$ess_by_mcmcse[1006])/val$eHMC_Summary$Comp,
                             min(val$eHMC_Summary$statistics$ess_by_coda[1:1000])/val$eHMC_Summary$Comp,
                             (val$eHMC_Summary$statistics$ess_by_coda[1004])/val$eHMC_Summary$Comp,
                             (val$eHMC_Summary$statistics$ess_by_coda[1005])/val$eHMC_Summary$Comp,
                             (val$eHMC_Summary$statistics$ess_by_coda[1006])/val$eHMC_Summary$Comp,
                             val$eHMC_Summary$statistics$ess_by_mcmcse[1007]/val$eHMC_Summary$Comp,
                             val$eHMC_Summary$statistics$ess_by_coda[1007]/val$eHMC_Summary$Comp,
                             val$eHMC_Summary$statistics$ESJD_x/val$eHMC_Summary$Comp,
                             val$eHMC_Summary$statistics$ESJD_para/val$eHMC_Summary$Comp,
                             val$eHMC_Summary$statistics$ESJD_lp/val$eHMC_Summary$Comp,
                             val$eHMC_Summary$Comp, val$eHMC_Summary$epsilon,
                             mean(val$LearnL_Summary$L_emp),
                             val$eHMC_Summary$accept)
    MetriceHMCq[[k]][m,] <- c(min(val$eHMCq_Summary$statistics$ess_by_mcmcse[1:1000])/val$eHMCq_Summary$Comp,
                              (val$eHMCq_Summary$statistics$ess_by_mcmcse[1004])/val$eHMCq_Summary$Comp,
                              (val$eHMCq_Summary$statistics$ess_by_mcmcse[1005])/val$eHMCq_Summary$Comp,
                              (val$eHMCq_Summary$statistics$ess_by_mcmcse[1006])/val$eHMCq_Summary$Comp,
                              min(val$eHMCq_Summary$statistics$ess_by_coda[1:1000])/val$eHMCq_Summary$Comp,
                              (val$eHMCq_Summary$statistics$ess_by_coda[1004])/val$eHMCq_Summary$Comp,
                              (val$eHMCq_Summary$statistics$ess_by_coda[1005])/val$eHMCq_Summary$Comp,
                              (val$eHMCq_Summary$statistics$ess_by_coda[1006])/val$eHMCq_Summary$Comp,
                              val$eHMCq_Summary$statistics$ess_by_mcmcse[1007]/val$eHMCq_Summary$Comp,
                              val$eHMCq_Summary$statistics$ess_by_coda[1007]/val$eHMCq_Summary$Comp,
                              val$eHMCq_Summary$statistics$ESJD_x/val$eHMCq_Summary$Comp,
                              val$eHMCq_Summary$statistics$ESJD_para/val$eHMCq_Summary$Comp,
                              val$eHMCq_Summary$statistics$ESJD_lp/val$eHMCq_Summary$Comp,
                              val$eHMCq_Summary$Comp, val$eHMCq_Summary$epsilon,
                              quantile(val$LearnL_Summary$L_emp,0.95),
                              val$eHMCq_Summary$accept)
    MetriceHMCu[[k]][m,] <- c(min(val$eHMCu_Summary$statistics$ess_by_mcmcse[1:1000])/val$eHMCu_Summary$Comp,
                              (val$eHMCu_Summary$statistics$ess_by_mcmcse[1004])/val$eHMCu_Summary$Comp,
                              (val$eHMCu_Summary$statistics$ess_by_mcmcse[1005])/val$eHMCu_Summary$Comp,
                              (val$eHMCu_Summary$statistics$ess_by_mcmcse[1006])/val$eHMCu_Summary$Comp,
                              min(val$eHMCu_Summary$statistics$ess_by_coda[1:1000])/val$eHMCu_Summary$Comp,
                              (val$eHMCu_Summary$statistics$ess_by_coda[1004])/val$eHMCu_Summary$Comp,
                              (val$eHMCu_Summary$statistics$ess_by_coda[1005])/val$eHMCu_Summary$Comp,
                              (val$eHMCu_Summary$statistics$ess_by_coda[1006])/val$eHMCu_Summary$Comp,
                              val$eHMCu_Summary$statistics$ess_by_mcmcse[1007]/val$eHMCu_Summary$Comp,
                              val$eHMCu_Summary$statistics$ess_by_coda[1007]/val$eHMCu_Summary$Comp,
                              val$eHMCu_Summary$statistics$ESJD_x/val$eHMCu_Summary$Comp,
                              val$eHMCu_Summary$statistics$ESJD_para/val$eHMCu_Summary$Comp,
                              val$eHMCu_Summary$statistics$ESJD_lp/val$eHMCu_Summary$Comp,
                              val$eHMCu_Summary$Comp,val$eHMCu_Summary$epsilon,
                              mean(val$LearnL_Summary$L_emp)/2 + 0.5,
                              val$eHMCu_Summary$accept)
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
  ggtitle("Stochastic Volatility: Min ESS of" ~x) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.001))


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
  ggtitle("Stochastic Volatility: ESS of" ~phi) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.001))



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
  ggtitle("Stochastic Volatility: ESS of" ~kappa) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.0005))

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
  ggtitle("Stochastic Volatility: ESS of" ~sigma) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.0004))


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
  ggtitle("Stochastic Volatility: ESS of" ~x) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.001))


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
  ggtitle("Stochastic Volatility: ESS of" ~phi) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.0008))

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
  ggtitle("Stochastic Volatility: ESS of" ~kappa) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.0004))


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
  ggtitle("Stochastic Volatility: ESS of" ~sigma) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.0003))


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
  ggtitle("Stochastic Volatility: ESJD of" ~x) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,4))

n <- 12
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
  ylab("ESJD per gradient") + 
  ggtitle("Stochastic Volatility: ESJD of parameters") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,0.000025))


n <- 15
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
  ggtitle("Stochastic Volatility (identity):" ~epsilon) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) +
  ylim(c(0,0.04))



n <- 16
IndTemp <- n
ESSNUTS <- matrix(0,nrow=8,ncol=40)
ESS1 <- matrix(0,nrow=8,ncol=40)
ESS2 <- matrix(0,nrow=8,ncol=40)
ESS3 <- matrix(0,nrow=8,ncol=40)
for(k in 1:8)
{
  ESSNUTS[k,] <- MetricNUTS[[k]][,IndTemp] * MetricNUTS[[k]][,15]
  ESS1[k,] <- MetriceHMC[[k]][,IndTemp] * MetriceHMC[[k]][,15]
  ESS2[k,] <- MetriceHMCq[[k]][,IndTemp] * MetriceHMCq[[k]][,15]
  ESS3[k,] <- MetriceHMCu[[k]][,IndTemp] * MetriceHMCu[[k]][,15]
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
  ggtitle("Integration Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = function(x) format(x, scientific = TRUE, digits=3)) + 
  ylim(c(0,6.6))
