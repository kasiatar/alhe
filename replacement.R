generationalReplacemen<-function(p, o)
{
  return (o)
}

eliteReplacement<-function(p, o)
{
  minP<-1
  for (i in 2:length(p)){
    if(p[[i]]$quality<p[[minP]]$quality){
      minP<-i
    }
  }
  maxO<-1
  for (i in 2:length(o)){
    if(o[[i]]$quality>o[[maxO]]$quality){
      maxO<-i
    }
  }
  if(o[[maxO]]$quality>p[[minP]]$quality){
    o[maxO]<-p[minP]
  }
  return (o)
}