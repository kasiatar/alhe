exportAverageResultToFile<-function(results, name)
{
  result<-monotonicQualities(averageResult(results))
  write.csv(result, file=paste("trunk\\results\\",name,".csv",sep=""), row.names=FALSE)
}

monotonicQualities<-function(result)
{
  bestQ<-result[[1]]
  for(j in 2:length(result))
  {
    if(result[[j]]<bestQ)
    {
      result[[j]]<-bestQ
    }
    else
    {
      bestQ<-result[[j]]
    }
  }
  return (result)
}

averageResult<-function(results)
{
  averageRes<-array(-1, length(results[[1]]))
  for(i in 1:length(averageRes))
  {
    for(j in 1:length(results))
    {
      averageRes[[i]]<-averageRes[[i]]+results[[j]][[i]]
    }
    averageRes[[i]]<-averageRes[[i]]/length(results)
  }
  return (averageRes)
}

exportAll<-function()
{
  exportAverageResultToFile(p1ww1us5pc00Results,"p1ww1us5pc00ResultAverage")
  exportAverageResultToFile(p1ww1us5pc02Results,"p1ww1us5pc02ResultAverage")
  exportAverageResultToFile(p1ww1us5pc07Results,"p1ww1us5pc07ResultAverage")
  exportAverageResultToFile(p1ww1us5pc10Results,"p1ww1us5pc10ResultAverage")
  exportAverageResultToFile(p1ww1ws5pc00Results,"p1ww1ws5pc00ResultAverage")
  exportAverageResultToFile(p1ww1ws5pc02Results,"p1ww1ws5pc02ResultAverage")
  exportAverageResultToFile(p1ww1ws5pc07Results,"p1ww1ws5pc07ResultAverage")
  exportAverageResultToFile(p1ww1ws5pc10Results,"p1ww1ws5pc10ResultAverage")
  exportAverageResultToFile(p2ww2us5pc00Results,"p2ww2us5pc00ResultAverage")
  exportAverageResultToFile(p2ww2us5pc02Results,"p2ww2us5pc02ResultAverage")
  exportAverageResultToFile(p2ww2us5pc07Results,"p2ww2us5pc07ResultAverage")
  exportAverageResultToFile(p2ww2us5pc10Results,"p2ww2us5pc10ResultAverage")
  exportAverageResultToFile(p2ww2ws5pc00Results,"p2ww2ws5pc00ResultAverage")
  exportAverageResultToFile(p2ww2ws5pc02Results,"p2ww2ws5pc02ResultAverage")
  exportAverageResultToFile(p2ww2ws5pc07Results,"p2ww2ws5pc07ResultAverage")
  exportAverageResultToFile(p2ww2ws5pc10Results,"p2ww2ws5pc10ResultAverage")
}

presentResultsPc<-function(pc00, pc02, pc07, pc10)
{
  averageResPc00<-monotonicQualities(averageResult(pc00))
  averageResPc02<-monotonicQualities(averageResult(pc02))
  averageResPc07<-monotonicQualities(averageResult(pc07))
  averageResPc10<-monotonicQualities(averageResult(pc10))
  highQ<-max(max(averageResPc00),max(averageResPc02),max(averageResPc07),max(averageResPc10))
  lowQ<-max(min(averageResPc00),min(averageResPc02),min(averageResPc07),min(averageResPc10))
  plot(0, 0, xlim=c(0,length(averageResPc00)), ylim=c(lowQ,highQ), type="n", main="Jakość najlepszego dotychczas punktu historii", xlab="numer punktu", ylab="maksymalna wartość funkcji celu")
  lines(1:length(averageResPc00), averageResPc00, col="red")
  lines(1:length(averageResPc02), averageResPc02, col="orange")
  lines(1:length(averageResPc07), averageResPc07, col="green")
  lines(1:length(averageResPc10), averageResPc10, col="blue")
}