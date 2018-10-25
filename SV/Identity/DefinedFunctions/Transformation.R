Transformation <- function(theta,feature_names)
{
    TransformedTheta <- cbind(theta, (exp(theta[,1001])-1)/(exp(theta[,1001])+1),
    exp(theta[,1002]), exp(0.5*theta[,1003]), -apply(theta,1,U))
    
    colnames(TransformedTheta) <- feature_names
    return(TransformedTheta)
}
