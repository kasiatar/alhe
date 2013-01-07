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