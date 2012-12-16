evolutionaryVariationTable<-function(p, replacement, c = 0.7)
{
  if(runif(1) < c){
    return (replacement(p, switchMutation(pmxCrossover(p))))
  }
  else{
    return (replacement(p, switchMutation(p)))
  }
}