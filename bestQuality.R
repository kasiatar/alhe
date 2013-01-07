bestQuality<-function(history)
{
  bestQ<-history[[1]]$quality
  for(i in 2:length(history)){
    if(history[[i]]$quality<bestQ){
      bestQ<-history[[i]]$quality
    }
  }
  return (bestQ)
}