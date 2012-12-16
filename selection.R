tournamentSelection<-function(p, s=2)
{
  t<-array(list(NULL), 100)
  for(i in 1:length(t)){
    playersIndices<-sample.int(length(p), s, TRUE)
    winnerIndex<-1
    for(j in 2:s){
      if(p[[winnerIndex]]$quality<p[[playersIndices[j]]]$quality){
        winnerIndex<-playersIndices[j]
      }
    }
    t[i]<-p[j]
  }
  return (t)
}

rankSelection<-function(p, s=1/7)
{
    
}