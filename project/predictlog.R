#Cost Function
predictlog <- function(X,Y,theta)
{
  predictions=sigmoid(X%*%theta)
  #lower the threshold from 0.5 to 0.471 to optimize predictions
  predictions=as.numeric(predictions>0.471)
  confusionmatrix=table(Y,predictions)
  
  return(confusionmatrix)
}