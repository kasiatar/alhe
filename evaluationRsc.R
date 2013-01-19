evaluationTableRsk<-function(coordinates,profity, wagi)
{
  wSum <- 0
  pSum <- 0
  i <-  1
  while(TRUE){
    #|| (i>=(length(coordinates)))){
    ind <- coordinates[[i]]
    wSum <- wSum + wagi[[1]][[ind]]
    if((wSum > 100)){
      return (pSum)
    }
    pSum <- pSum + profity [[1]][[ind]]
    i<-i+1
  }
}