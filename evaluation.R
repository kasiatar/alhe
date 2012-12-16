evaluationTable<-function(point$coordinates)
{
  quality<-0
  for(i in 1:length(point$coordinates)-1){
    for(j in (i+1):length(point$coordinates)){
      if(point$coordinates[[i]] > point$coordinates[[j]]){
        quality<-quality+1
      }
    }
  }
  return (quality)
}