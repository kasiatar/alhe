p = initializationTable(p)
p = evaluateList(p, evaluationTable)
minP = 1
for(i in 2:length(p)){
  if(p[[i]]$quality<p[[minP]]$quality){
    minP<-i
  }
}
print(p[[minP]])
for(i in 1:100){
  print(i)
  print(p[[minP]]$quality)
  p = tournamentSelection(p)
  p = evolutionaryVariationTable(p, eliteReplacement, 0.0)
  p = evaluateList(p, evaluationTable)
}
minP = 1
for(i in 2:length(p)){
  if(p[[i]]$quality<p[[minP]]$quality){
    minP<-i
  }
}
print(p[[minP]])