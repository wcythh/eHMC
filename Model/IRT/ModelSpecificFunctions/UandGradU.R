U <- function(eta)
{
    phi_theta <- eta[1]
    theta <- eta[2:101]
    phi_a <- eta[102]
    phi <- eta[103:122]
    a <- exp(phi)
    mu_b <- eta[123]
    phi_b <- eta[124]
    b <- eta[125:144]
    val <- 0
    for(i in 1:I0)
    {
        val0 <- a[i] * (theta-b[i])
        val_check <- sum(log(1+exp(val0)))
        if(sum(val_check == Inf) >0 | sum(val_check == -Inf)>0| sum(is.na(val_check))>0 )
        {
            return(NA)
        }
        val <- val + sum(log(1+exp(val0))) - sum(y0[i,] * val0)
    }
    val <- val + log(1+0.25*exp(2*phi_theta)) -
    phi_theta + sum(theta^2)/(2*exp(2*phi_theta)) +
    log(1+0.25*exp(2*phi_a)) - phi_a + sum(phi^2)/(2*exp(2*phi_a)) + mu_b^2/50 +
    log(1+0.25*exp(2*phi_b)) - phi_b + sum((b-mu_b)^2)/(2*exp(2*phi_b)) +
    J0*phi_theta + I0*phi_a + I0*phi_b
    return(val)
}

grad_U <- function(eta)
{
    phi_theta <- eta[1]
    theta <- eta[2:101]
    phi_a <- eta[102]
    phi <- eta[103:122]
    a <- exp(phi)
    mu_b <- eta[123]
    phi_b <- eta[124]
    b <- eta[125:144]
    
    ##check Inf, -Inf, NA
    val_check <- c(exp(-2*phi_theta), exp(2*phi_theta), exp(-2*phi_a), exp(2*phi_a), exp(-2*phi_b),
    exp(2*phi_b))
    if(sum(val_check == Inf) >0 | sum(val_check == -Inf)>0| sum(is.na(val_check))>0 )
    {
        return(rep(NA, 144))
    }

    ## Compute grad phi_theta
    g_phi_theta <- 0.5/(0.25+exp(-2*phi_theta)) - 1 - sum(theta^2)/(exp(2*phi_theta)) + J0
    
    ## Compute grad theta_j
    g_theta <- rep(0,J0)
    for(j in 1:J0)
    {
        val0 <- a *(theta[j] - b)
        val_check <- exp(-val0)
        if(sum(val_check == Inf) >0 | sum(val_check == -Inf)>0| sum(is.na(val_check))>0 )
        {
            return(rep(NA, 144))
        }
        g_theta[j] <- sum(a/(1+exp(-val0))) - sum(y0[,j] * a) + theta[j]/(exp(2*phi_theta))
    }
    
    ## Compute grad phi_a
    g_phi_a <- 0.5/(0.25+exp(-2*phi_a)) - 1 - sum(phi^2)/exp(2*phi_a) + I0
    
    ## Compute grad phi_i
    g_phi <- rep(0,I0)
    for(i in 1:I0)
    {
        val0 <- a[i] * (theta - b[i])
        val_check <- exp(-val0)
        if(sum(val_check == Inf) >0 | sum(val_check == -Inf)>0| sum(is.na(val_check))>0 )
        {
            return(rep(NA, 144))
        }
        g_phi[i] <- sum(val0/(1+exp(-val0))) - sum(y0[i,]*val0) + phi[i]/(exp(2*phi_a))
    }
    ## Compute grad mu_b
    g_mu_b <- mu_b/25 - sum(b-mu_b)/exp(2*phi_b)
    
    ## Compute grad phi_b
    g_phi_b <- 0.5/(0.25+exp(-2*phi_b)) - 1 - sum((b-mu_b)^2)/exp(2*phi_b) + I0
    
    ## Compute grad b_i
    g_b <- rep(0,I0)
    for(i in 1:I0)
    {
        val0 <- a[i] *(theta - b[i])
        val_check <- exp(-val0)
        if(sum(val_check == Inf) >0 | sum(val_check == -Inf)>0| sum(is.na(val_check))>0 )
        {
            return(rep(NA, 144))
        }
        g_b[i] <- sum(-a[i]/(1+exp(-val0))) + sum(y0[i,])*a[i] + (b[i]-mu_b)/exp(2*phi_b)
    }
    
    return(c(g_phi_theta, g_theta, g_phi_a, g_phi, g_mu_b, g_phi_b, g_b))
}
