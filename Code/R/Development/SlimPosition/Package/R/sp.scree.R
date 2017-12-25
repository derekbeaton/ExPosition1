sp.scree <- function(eigs,main="Scree"){

  taus <- eigs/sum(eigs)*100

  plot(taus,type="l",ylab="% explained", xlab="Component", main=main)
  #points(1:length(taus),cumsum(taus),pch=20)
  points(1:length(taus),taus,pch=20)

}
