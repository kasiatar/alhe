plot(0, 0, xlim=c(0,length(h1p1pc0)), ylim=c(0,5000), type="n", main="Jakość kolejnych punktów historii", xlab="numer punktu", ylab="wartość funkcji celu")
lines(1:length(h1p2pc0), lapply(h1p2pc0, function(x){as.numeric(x$quality)}), col="red")
lines(1:length(h1p1pc0), lapply(h1p1pc0, function(x){as.numeric(x$quality)}), col="blue")
lines(1:length(h1p3pc0), lapply(h1p3pc0, function(x){as.numeric(x$quality)}), col="green")