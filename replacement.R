generationalReplacemen<-function(p, o)
{
  return (o)
}

eliteReplacement<-function(p, o)
{
  maxP<-1
  for (i in 2:length(p)){
    if(p[[maxP]]$quality<p[[i]]$quality){
      maxP<-i
    }
  }
  minO<-1
  for (i in 2:length(o)){
    if(o[[minO]]$quality>o[[i]]$quality){
      minO<-i
    }
  }
  if(p[maxP]>o[minO]){
    o[minO]<-p[maxP]
  }
  return (o)
}