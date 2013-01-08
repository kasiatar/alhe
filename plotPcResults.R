plot(0,0, xlim=c(500,1500), ylim=c(0.0, 1.0), type="n", main="Dystrybuanty", xlab="wartość funkcji celu", ylab="prawdopodobieństwo")
lines(p1pc10resultsDistribution, col="blue")
lines(p1pc00resultsDistribution, col="red")