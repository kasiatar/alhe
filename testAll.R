testAll<-function(){
  results<-array(-1, 20)
  for(i in 1:20){
    tempHistory<-metaheuristicRun(initializationTable, p1, terminationHistory, evaluationTable)
    quality<-bestQuality(tempHistory)
    results[[i]]<-quality
    print(quality)
  }
  return (results)
}

testAll2<-function(){
  results<-array(-1, 25)
  for(i in 1:25){
    set.seed(primes[[1]][[i]])
    tempHistory<-metaheuristicRun2(initializationTable, p1, terminationHistory, evaluationTableRsk)
    quality<-bestQualityMax(tempHistory)
    results[[i]]<-quality
    print(quality)
  }
  return (results)
}