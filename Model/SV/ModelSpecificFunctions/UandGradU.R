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
