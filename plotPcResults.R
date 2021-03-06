#PROBLEM SORTOWANIA

#parametr turnieju s=2
plot(0,0, xlim=c(500,1500), ylim=c(0.0, 1.0), type="n", main="Dystrybuantydla parametru turnieju s=2", xlab="wartość funkcji celu", ylab="prawdopodobieństwo")
lines(p1pc00resultsDistribution, col="blue")
lines(p1pc02resultsDistribution, col="violet")
lines(p1pc04resultsDistribution, col="red")
lines(p1pc07resultsDistribution, col="yellow")
lines(p1pc10resultsDistribution, col="green")

#parametr turnieju s=3
plot(0,0, xlim=c(500,800), ylim=c(0.0, 1.0), type="n", main="Dystrybuanty dla parametru turnieju s=3", xlab="wartość funkcji celu dla problemu sortowania", ylab="prawdopodobieństwo")
lines(p1t3pc00Distribution, col="blue")
lines(p1t3pc02Distribution, col="violet")
lines(p1t3pc04Distribution, col="red")
lines(p1t3pc07Distribution, col="yellow")
lines(p1t3pc10Distribution, col="green")

#parametr turnieju s=5
plot(0,0, xlim=c(300,700), ylim=c(0.0, 1.0), type="n", main="Dystrybuanty dla parametru turnieju s=5", xlab="wartość funkcji celu", ylab="prawdopodobieństwo")
lines(p1s5pc00resultsDistribution, col="blue")
lines(p1s5pc02resultsDistribution, col="violet")
lines(p1s5pc04resultsDistribution, col="red")
lines(p1s5pc07resultsDistribution, col="yellow")
lines(p1s5pc10resultsDistribution, col="green")


#PROBLEM PLECAKOWY
# p1w, w1u
plot(0,0, xlim=c(220,290), ylim=c(0.0, 1.0), type="n", main="Dystrybuanty parametru turnieju s=2", xlab="wartość funkcji celu dla problemu plecakowego", ylab="prawdopodobieństwo")
lines(prt2pc00Distribution, col="blue")
lines(prt2pc02Distribution, col="violet")
lines(prt2pc04Distribution, col="red")
lines(prt2pc07Distribution, col="yellow")
lines(prt2pc10Distribution, col="green")

# p1w, w1w
plot(0,0, xlim=c(160,195), ylim=c(0.0, 1.0), type="n", main="Dystrybuanty parametr turnieju s=2", xlab="wartość funkcji celu dla problemu plecakowego p1w, w1w", ylab="prawdopodobieństwo")
lines(prWt2pc00Distribution, col="blue")
lines(prWt2pc02Distribution, col="violet")
lines(prWt2pc04Distribution, col="red")
lines(prWt2pc07Distribution, col="yellow")
lines(prWt2pc10Distribution, col="green")