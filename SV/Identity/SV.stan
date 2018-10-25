# Removing the constraints of parameters by adding Jacobian adjustment 
# in order to check the correctness of hand-written U and grad_U
data
{
    int<lower=0> T1;
    vector[T1] y;
}
parameters
{
    vector[T1] x;
    real alpha;
    real beta;
    real gamma;
}

transformed parameters {
    real phi = (exp(alpha) - 1)/(exp(alpha) + 1);
    real kappa = exp(beta);
    real sigma = exp(0.5*gamma);
}
model
{
    x[1] ~ normal(0, sigma/sqrt(1-phi*phi));
    for(t in 2:T1)
        x[t] ~ normal(phi * x[t-1], sigma);
    for(t in 1:T1)
        y[t] ~ normal(0,kappa*exp(0.5*x[t]));
    target += 20*alpha - 21.5*log(exp(alpha)+1);
    target += -5*gamma - 0.25*exp(-gamma);

}



