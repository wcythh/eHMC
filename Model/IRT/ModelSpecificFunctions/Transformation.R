###Transformation
Transformation <- function(theta, feature_names)
{
    TransformedTheta <- cbind(theta, exp(theta[,1]),exp(theta[,102]),exp(theta[,124]),exp(theta[,103:122]),-apply(theta,1,U))
  
  colnames(TransformedTheta) <- feature_names
  return(TransformedTheta)
}

