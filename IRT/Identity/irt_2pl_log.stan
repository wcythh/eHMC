// Based on stan-dev's code
// https://github.com/stan-dev/stat_comp_benchmarks/tree/master/benchmarks/irt_2pl
// Removing the constraints of parameters to check the correctness of hand-written U and grad_U functions.
data {
  int<lower=0> I;
  int<lower=0> J;
  int<lower=0, upper=1> y[I, J];
}

parameters {
  real phi_theta;
  vector[J] theta;

  real phi_a;
  vector[I] phi;
  
  real mu_b;
  real phi_b;
  vector[I] b;
}

transformed parameters{
  real sigma_theta = exp(phi_theta);
  real sigma_a = exp(phi_a);
  real sigma_b = exp(phi_b);
  vector[I] a = exp(phi);

}

model {
  //sigma_theta ~ cauchy(0, 2);
  theta ~ normal(0, sigma_theta); 

  //sigma_a ~ cauchy(0, 2);
  //a ~ lognormal(0, sigma_a);
  phi ~ normal(0,sigma_a);

  mu_b ~ normal(0, 5);
  //sigma_b ~ cauchy(0, 2);
  b ~ normal(mu_b, sigma_b);

  for (i in 1:I)
  {
    y[i] ~ bernoulli_logit(a[i] * (theta - b[i]));
  }

  target += phi_theta - log(1+0.25*exp(2*phi_theta)) + phi_a - log(1+0.25*exp(2*phi_a)) + phi_b - log(1+0.25*exp(2*phi_b));
}
