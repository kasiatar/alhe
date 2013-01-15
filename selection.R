tournamentSelection<-function(p, s=3)
{
  t<-array(list(NULL), length(p))
  for(i in 1:length(t)){
    playersIndices<-sample.int(length(p), s, TRUE)
    winnerIndex<-playersIndices[1]
    for(j in 2:s){
      if(p[[playersIndices[j]]]$quality<p[[winnerIndex]]$quality){
        winnerIndex<-playersIndices[j]
      }
    }
    t[i]<-p[winnerIndex]
  }
  return (t)
}

truncationSelection<-function(p, s=1/7)
{
    p<-p[order(sapply(p, function(x){as.numeric(x$quality)}))]
    cutIndex<-as.integer(s*length(p))
    t<-sample(p[1:cutIndex], length(p), replace=TRUE)
    return (t)
}

tournamentSelection2<-function(p, s=3)
{
  t<-array(list(NULL), length(p))
  for(i in 1:length(t)){
    playersIndices<-sample.int(length(p), s, TRUE)
    winnerIndex<-playersIndices[1]
    for(j in 2:s){
      if(p[[playersIndices[j]]]$quality>p[[winnerIndex]]$quality){
        winnerIndex<-playersIndices[j]
      }
    }
    t[i]<-p[winnerIndex]
  }
  return (t)
}