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
  results<-array(list(NULL), 25)
  
  startPoints<-list(populationLength=500, arrayLength=500)
  
  for(i in 1:25){
    set.seed(primes[[1]][[i]])
    
    tempHistory<-metaheuristicRun2(initializationPopulation, startPoints, terminationHistory, evaluationTableRsk)

    results[[i]]<-qualitiesFromHistory(tempHistory)
    
    bestQ<-bestQualityMax(tempHistory)
    
    print(bestQ)
  }
  return (results)
}