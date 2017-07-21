sp.scree <- function(eigs){

  taus <- eigs/sum(eigs)*100

  plot(taus,type="l",ylab="% explained", xlab="Component", main="Scree")
  points(1:length(taus),cumsum(taus),pch=20)
  points(1:length(taus),taus,pch=20)

}
