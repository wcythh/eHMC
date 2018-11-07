U <- function(theta)
{
    val <- - log_prob(fit, theta)
    return(val)
}

grad_U <- function(theta)
{
    val <- - grad_log_prob(fit, theta)[1:d]
    return(val)
}
