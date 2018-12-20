data
{
    int<lower=0> d;
    int<lower=0> N;
    matrix[N, d] R;
    int S[N];
}
parameters
{
    vector[d] beta;
}
model
{
    for(n in 1:N)
       S[n] ~ bernoulli_logit((R[n,:]*beta));
}
