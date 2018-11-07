###Transformation
SIR <- function(t,y,theta)
{
    dydt <- rep(0,4)
    dydt[1] <- -theta[1]*y[4]/(y[4]+theta[2]) *y[1]
    dydt[2] <- theta[1]*y[4]/(y[4]+theta[2])*y[1] - theta[3]*y[2]
    dydt[3] <- theta[3]*y[2]
    dydt[4] <- theta[4]*y[2] - theta[5]*y[4]
    return(list(dydt))
}

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

generated_quantity <- function(phi)
{
    theta <- exp(phi)
    y_hat <- Integrate_ode45(theta)
    y_hat <- y_hat[-1,]
    return(as.vector(y_hat))
}

Transformation <- function(theta, feature_names)
{
  TransformedTheta <- cbind(theta,exp(theta),t(apply(theta,1,generated_quantity)),-apply(theta,1,U))
  
  colnames(TransformedTheta) <- feature_names
  return(TransformedTheta)
}

