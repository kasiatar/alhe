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
    p2<-initializationTable1()
    tempHistory<-metaheuristicRun2(initializationTable, p2, terminationHistory, evaluationTableRsk)
    #print(length(p2))
    quality<-bestQualityMax(tempHistory)
    results[[i]]<-quality
    print(quality)
  }
  return (results)
}