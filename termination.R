terminationHistory<-function(history, model)
{
  if(length(history)>10000){
    return (TRUE)
  }
  return (FALSE)
}

terminationModel<-function(history, model)
{
  if(model$bestPoint$quality<800){
    return (TRUE)
  }
  return (FALSE)
}