evaluationTableRsk<-function(coordinates,profity, wagi)
{
  wSum <- 0
  pSum <- 0
  i <-  1
  while((wSum <= 100)){
    #|| (i>=(length(coordinates)))){
    ind <- coordinates[[i]]
    wSum <- wSum + wagi[[1]][[ind]]
    pSum <- pSum + profity [[1]][[ind]]
    i<-i+1
  }
  return (pSum)
}