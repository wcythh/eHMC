###Transformation
Transformation <- function(theta, feature_names)
{
  TransformedTheta <- cbind(theta, -apply(theta,1,U))
  
  colnames(TransformedTheta) <- feature_names
  return(TransformedTheta)
}
