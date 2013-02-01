qualitiesFromHistory<-function(history){
  qualities<-array(-1, length(history))
  for(i in 1:length(history)){
    qualities[[i]]<-history[[i]]$quality
  }
  return (qualities)
}