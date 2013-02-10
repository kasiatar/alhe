transformToSortedBestArray<-function(results)
{
  sortedRes<-array(NA, length(results))
  for(j in 1:length(results)){
    sortedTemp<-sort(results[[j]],  decreasing = TRUE)
    sortedRes[[j]]<-sortedTemp[[1]]
  }
  #sortedBestRes<-array(list(NULL), length(results))
  sortedBestRes<-sort(sortedRes,  decreasing = FALSE)
  return (sortedBestRes)
}

retKwart<-function(sBestRes)
{
  kwartyle<-list()
  kwartyle[1]<-sBestRes[[1]]
  kwartyle[2]<-sBestRes[[7]]
  kwartyle[3]<-sBestRes[[13]]
  kwartyle[4]<-sBestRes[[19]]
  kwartyle[5]<-sBestRes[[25]]

  return (kwartyle)
}

presentDistributionResultsPc<-function(pc00, pc02, pc07, pc10)
{
  BestPc00<-ecdf(transformToSortedBestArray(pc00))
  BestPc02<-ecdf(transformToSortedBestArray(pc02))
  BestPc07<-ecdf(transformToSortedBestArray(pc07))
  BestPc10<-ecdf(transformToSortedBestArray(pc10))
  print("Kwartyle dla pc00")
  print(retKwart(transformToSortedBestArray(pc00)))
  print("Kwartyle dla pc02")
  print(retKwart(transformToSortedBestArray(pc02)))
  print("Kwartyle dla pc07")
  print(retKwart(transformToSortedBestArray(pc07)))
  print("Kwartyle dla pc10")
  print(retKwart(transformToSortedBestArray(pc10)))
  highQ<-max(max(transformToSortedBestArray(pc00)),max(transformToSortedBestArray(pc02)),max(transformToSortedBestArray(pc07)),max(transformToSortedBestArray(pc10)))
  lowQ<-min(min(transformToSortedBestArray(pc00)),min(transformToSortedBestArray(pc02)),min(transformToSortedBestArray(pc07)),min(transformToSortedBestArray(pc10)))
  plot(0, 0, xlim=c(lowQ,highQ), ylim=c(0.0, 1.0), type="n", 
       main="Dystrybuanta plecak p1w, w1u", 
       xlab="wartosc funkcji celu", ylab="prawdopodobieństwo")
  lines(BestPc00, col="blue")
  lines(BestPc02, col="violet")
  lines(BestPc07, col="yellow")
  lines(BestPc10, col="green")
}