initializationTable<-function(startPoints){
  return (startPoints)
}

initializationTable1<-function(startPoints)
{
  p<-array(list(NULL), 100)
  for(i in 1:100){
    p[[i]]<-list(coordinates=sample.int(100, 100), quality=NA)
  }
  return (p)
}

initializationTable2<-function(startPoints)
{
  point<-list(coordinates=array(1:100), quality=NA)
  point$coordinates<-sort(point$coordinates, decreasing=TRUE)
  p<-array(list(NULL), 100)
  for(i in 1:100){
    p[[i]]<-point
  }
  return (p)
}

initializationTable3<-function(startPoints)
{
  p<-array(list(NULL), 100)
  for(i in 1:100){
    first<-sample.int(50, 50)
    second<-sample.int(50, 50)
    for(j in 1:50){
      second[j]<-second[j]+50
    }
    point<-list(coordinates=c(first, second), quality=NA)
    p[[i]]<-point
  }
  return (p)
}

initializationPopulation<-function(startPoints)
{
  p<-array(list(NULL), startPoints$populationLength)
  for(i in 1:(startPoints$populationLength)){
    p[[i]]<-list(coordinates=sample.int(startPoints$arrayLength, startPoints$arrayLength), quality=NA)
  }
  return (p)
}