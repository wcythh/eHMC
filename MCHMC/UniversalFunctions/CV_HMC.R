###################### PDMP ###################### 
## X_Trace, K_Trace
## Index
## V_Left, V_Right
## Direction: 1 ==> forward; -1 ==> backward
## X_Propose, K_Propose
## K_Current
## X_Len

#### parameters 
#eta <- 0.1
#epsilon <- epsilon0
#L_PDMP <- floor(quantile(Ind_trace,0.95))
#L <- 35
#### Initialization 
#Niter <- 1.85e4
CVHMC <- function(eta, L_emp,epsilon,M,theta,d,Niter,feature_names,U,grad_U,Leapfrog_dense)
{
  Xsim <- matrix(0,nrow=Niter,ncol=d)
  Xsim[1,] <- theta
  V_Current <- rnorm(d) * sqrt(M)
  
  X_Trace <- matrix(Xsim[1,],ncol=d)
  K_Current <- 0.5*sum(V_Current^2/M)
  K_Trace <- K_Current
  Index <- 1
  V_Left <- V_Current
  V_Right <- V_Current
  Direction <- 1
  X_Len <- 1
  Comp <- 0
  Ratio <- 0
  time_CVHMC <- proc.time()
  for(i in 2:Niter)
  {
    L_r <- sample(L_emp, size=1)
    L_r <- ceiling(L_r/3)
    if(runif(1) < eta)
    {
      #L_r <- sample(1:L_PDMP,size=1)
      X_Len <- L_r + 1
      Comp <- Comp + L_r 
      
      V_Current <- rnorm(d) * sqrt(M)
      K_Current <- 0.5*sum(V_Current^2/M)
      
      XV_Temp <- Leapfrog_dense(U,grad_U,epsilon,L_r,M,Xsim[i-1,],V_Current)
      V_Left <- XV_Temp$V[1,]
      V_Right <- XV_Temp$V[L_r+1,]
      X_Trace <- XV_Temp$X
      K_Trace <- apply(XV_Temp$V, 1, function(zz) (0.5*sum(zz^2/M)))
      
      if(XV_Temp$accept == 1)
      {
        Index <- L_r+1
        K_Current <- K_Trace[Index]
        Xsim[i,] <- XV_Temp$X[L_r+1,]
        Direction <- 1
        Ratio <- Ratio + 1
      }
      else
      {
        Index <- 1
        Xsim[i,] <- Xsim[i-1,]
        Direction <- -1
      }
    }
    else
    {
      #L_r <- sample(1:L_PDMP,size=1)
      if(Direction == 1)
      {
        if((X_Len - Index) >= L_r)
        {
          X_Propose <- X_Trace[Index+L_r,]
          K_Propose <- K_Trace[Index+L_r]
          
          delta <- U(Xsim[i-1,]) + K_Current - U(X_Propose) - K_Propose
          if(log(runif(1)) < delta)
          {
            Xsim[i,] <- X_Propose
            K_Current <- K_Propose
            Index <- Index + L_r
            Direction <- 1
            Ratio <- Ratio + 1
          }
          else
          {
            Xsim[i,] <- Xsim[i-1,]
            Direction <- -1
          }
        }
        else
        {
          L_delta <- L_r - (X_Len - Index)
          Comp <- Comp + L_delta 
          
          XV_Temp <- Leapfrog_dense(U,grad_U,epsilon,L_delta,M,X_Trace[X_Len,],V_Right)
          X_Trace <- rbind(X_Trace, XV_Temp$X[-1,])
          K_Trace <- c(K_Trace, as.vector(apply(XV_Temp$V,1,function(zz) 0.5*sum(zz^2/M)))[-1])
          V_Right <- XV_Temp$V[L_delta+1,]
          X_Len <- X_Len + L_delta
          
          if(XV_Temp$accept == 1)
          {
            Xsim[i,] <- XV_Temp$X[L_delta+1,]
            Index <- X_Len
            K_Current <- K_Trace[Index]
            Direction <- 1
            Ratio <- Ratio + 1
          }
          else
          {
            Xsim[i,] <- Xsim[i-1,]
            Direction <- -1
          }
        }
      }
      else
      {
        #L_r <- sample(1:L_PDMP,size=1)
        if(Index > L_r)
        {
          X_Propose <- X_Trace[Index-L_r,]
          K_Propose <- K_Trace[Index-L_r]
          
          delta <- U(Xsim[i-1,]) + K_Current - U(X_Propose) - K_Propose
          
          if(log(runif(1)) < delta)
          {
            Xsim[i,] <- X_Propose
            Index <- Index - L_r
            K_Current <- K_Propose
            Direction <- -1
            Ratio <- Ratio + 1
          }
          else
          {
            Xsim[i,] <- Xsim[i-1,]
            Direction <- 1
          }
        }
        else
        {
          L_delta <- L_r - (Index - 1)
          Comp <- Comp + L_delta
          
          XV_Temp <- Leapfrog_dense(U,grad_U,-epsilon,L_delta,M,X_Trace[1,],V_Left)
          
          V_Left <- XV_Temp$V[L_delta+1,]
          K_Trace <- c(rev(as.vector(apply(XV_Temp$V, 1, function(zz) 0.5*sum(zz^2/M)))[-1]),
                       K_Trace)
          X_Trace <- rbind(XV_Temp$X[(L_delta+1):2,], X_Trace)
          X_Len <- X_Len + L_delta
          
          if(XV_Temp$accept == 1)
          {
            Xsim[i,] <- XV_Temp$X[L_delta+1,]
            Index <- 1
            Direction <- -1
            K_Current <- 0.5*sum(V_Left^2)
            Ratio <- Ratio + 1
          }
          else
          {
            Xsim[i,] <- Xsim[i-1,]
            Index <- L_r + 1
            Direction <- 1
          }
        }
      }
    }
    if(i %% (Niter/10) == 0)
      print(paste("Iteration ", toString(100*i/Niter), "%", sep=""))
  }
  time_CVHMC <- proc.time() - time_CVHMC
  print(c((Ratio)/Niter,time_CVHMC))
  
  Xsamp <- Transformation(Xsim,feature_names)
  Xsummary <- SummaryFunction(Xsamp)
  return(list(Xsamp=Xsamp, Comp=Comp, epsilon=epsilon, Niter=Niter,
  ratio_empirical=Ratio/Niter, time=time_CVHMC,Xsummary=Xsummary))
  
}


