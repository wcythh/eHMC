data
{
    int<lower=0> d;
    matrix[d, d] A;
    vector[d] mu;
}
parameters
{
    vector[d] beta;
}
model
{
    beta ~ multi_normal(mu, A);
}
