initializationTable<-function()
{
  p<-array(list(NULL), 100)
  for(i in 1:100){
    p[[i]]<-list(coordinates=sample.int(100, 100), quality=NA)
  }
  return (p)
}