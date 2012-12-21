switchMutationTable<-function(point)
{
  j<-sample.int(length(point$coordinates), 2)
  temp<-point$coordinates[j[1]]
  point$coordinates[j[1]]<-point$coordinates[j[2]]
  point$coordinates[j[2]]<-temp
  return (point)
}