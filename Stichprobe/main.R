# von Mises Wahrscheinlichkeit


# Diese Funktion berechnet den Zusammhang zwischen Anzahl von Beobachtungen und Wahrscheinlichkeit. Je mehr Beobachtungen gemacht wird, desto mehr wird die relative Häufigkeit sich realer Wahrscheinlichkeit annähern.
my.VonMises <- function(n){
  x <- sample(1:10, size=n, replace=TRUE)
  p_mises <- cumsum(x==3)/(1:n)
  plot(p_mises, type='l' ,ylab=" ", xlab=" ")
  abline(h=1/10,col="red",lwd=2)
}

# Hier wird zuerst ein n Zahl gegeben, was die Grenze setzt. Danacah nach dieser Grenze wird jede Stichprobe bis zur Lange n berechnet, z.B. n sei 10 dann [1] [1,2] [1,2,3] {1,2,3,4} so bis zum 10. Danach wird bestimmt, ob je mehr Grenze wir erhöhen desto mehr sich die Stichprobenvarianz oder Mittelwert zur realen Varianz oder Mittelwert von Grungesamtheit im Beispiel von 10 ähnelt. Dieses Verfahren wird als empirisch gekennzeichnet, weil wir zufällig Stichproben ziehen.
my.Mittelwert <- function(n,legend=FALSE){
  grundgesamtheit <- c(1:10)
  mean.gg <- mean(grundgesamtheit)
  data <- sample(grundgesamtheit, size=n,replace=TRUE)
  mittelwerten <- c()
  
  for (i in 1:length(data)){
    mittelwerten <- append(mittelwerten, mean(data[1:i]))
  }
  
  plot(mittelwerten, type='l', xlab="n", ylab="Mittelwert",main="Mittelwert versus Stichprobenmittelwert", ylim=c(mean.gg-mean.gg*(1/5), mean.gg+mean.gg*(1/5)))
  abline(h=mean.gg, col="red",lwd=2, main="Mittelwert")
  
  if (legend==TRUE){
    legend(x="topright", legend=c("Stichprobenmittelwert", "Mittelwert"), fill=c("black","red"))
  }
}

my.Varianz <- function(n,legend=FALSE){
  grundgesamtheit <- c(1:10)
  var.gg <- var(grundgesamtheit)
  data <- sample(grundgesamtheit, size=n,replace=TRUE)
  varianzen <- c()
  
  for (i in 1:length(data)){
    varianzen <- append(varianzen, var(data[1:i]))
  }

  plot(varianzen, type='l', xlab="n", ylab="Varianz",main="Varianz versus Stichprobenvarianz")
  abline(h=var.gg, col="red", lwd=2, main="Varianz")
  
  if (legend==TRUE){
    legend(x="topright", legend=c("Stichprobenvarianz", "Varianz"), fill=c("black","red"))
  }
}

