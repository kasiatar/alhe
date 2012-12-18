evolutionaryVariationTable<-function(p, replacement, c = 0.7)
{
  if(runif(1) < c){
    k<-pmxCrossover(p)
  }
  else{
    k<-p
  }
  m<-switchMutationTable(k)
  o<-replacement(p, m)
  return (o)
}