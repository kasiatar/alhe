switchMutationTable<-function(p)
{
  for (i in 1:length(p)){
    j<-sample.int(length(p[[i]]$coordinates), 2)
    temp<-p[[i]]$coordinates[j[1]]
    p[[i]]$coordinates[j[1]]<-p[[i]]$coordinates[j[2]]
    p[[i]]$coordinates[j[2]]<-temp
  }
  return (p)
}