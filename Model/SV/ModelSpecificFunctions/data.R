###################### data ######################
set.seed(123456)
T1 <- 1000
kappa0 <- 0.65
sigma0 <- 0.15
phi0 <- 0.98
x0 <- rep(0,T1)
y <- rep(0,T1)
x0[1] <- rnorm(1,mean = 0, sd = (sigma0/sqrt(1-phi0^2)))
for(t in 2:T1)
{
    x0[t] <- phi0 * x0[t-1] + rnorm(1,mean=0,sd=sigma0)
}
y <- kappa0*exp(0.5*x0)*rnorm(T1)

Index1 <- 1:1000
Index2 <- 1001:1003
Index3 <- 1004:1006
Index4 <- "lp__"
