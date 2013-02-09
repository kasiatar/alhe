transformResultsList<-function(resultsList)
{
  for(i in 1:length(resultsList))
  {
    resultsList[[i]]<-monotonicQualitiesList(resultsList[[i]])
    resultsList[[i]]<-sortResults(resultsList[[i]])
    plotResults(resultsList[[i]])
  }
  
  #return (results)
}

sortResults<-function(results)
{
  sortedRes<-array(list(NULL), length(results))
  sortedTemp<-sort.int(sapply(results, function(x){as.numeric(x[[length(x)]])}), decreasing = TRUE, index.return=TRUE)
  for(i in 1:length(sortedTemp[[2]]))
  {
    sortedRes[[i]]<-results[[sortedTemp[[2]][[i]]]]
  }
  return (sortedRes)
}

monotonicQualitiesList<-function(qualitiesList)
{
  for(i in 1:length(qualitiesList))
  {
    bestQ<-qualitiesList[[i]][[1]]
    for(j in 2:length(qualitiesList[[i]]))
    {
      if(qualitiesList[[i]][[j]]<bestQ)
      {
        qualitiesList[[i]][[j]]<-bestQ
      }
      else
      {
        bestQ<-qualitiesList[[i]][[j]]
      }
    }
  }
  return(qualitiesList)
}

plotResults<-function(results)
{
  plot(0, 0, xlim=c(0,length(results[[1]])), ylim=c(0,results[[1]][[length(results[[1]])]]), type="n", main="Jakość najlepszego dotychczas punktu historii", xlab="numer punktu", ylab="maksymalna wartość funkcji celu")
#   legend(0,length(results[[1]])*0.8, c("Min", "Q1", "Q2", "Q3", "Max"),
#          lty=c(1,1,1,1,1), 
#          lwd=c(length(results[[1]])*0.01,length(results[[1]])*0.01), 
#          col=c("blue","green", "yellow", "orange", "red"))
  lines(1:length(results[[1]]), results[[1]], col="red")
  lines(1:length(results[[7]]), results[[7]], col="orange")
  lines(1:length(results[[13]]), results[[13]], col="yellow")
  lines(1:length(results[[19]]), results[[19]], col="green")
  lines(1:length(results[[25]]), results[[25]], col="blue")
}