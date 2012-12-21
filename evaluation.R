evaluationTable<-function(coordinates)
{
  quality<-0
  for(i in 1:(length(coordinates)-1)){
    for(j in (i+1):length(coordinates)){
      if(coordinates[[i]] > coordinates[[j]]){
        quality<-quality+1
      }
    }
  }
  return (quality)
}