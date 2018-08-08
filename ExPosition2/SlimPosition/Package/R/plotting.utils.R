## plotting utilities.

#' @export
ep.component.plot <- function(scores, axes=c(1,2), pch=20, col="mediumorchid4", line.col="grey80", lty=2, lwd=2,
                              main="Component scores",
                              xlab=paste0("Component ",axes[1]),
                              ylab=paste0("Component ",axes[2]),
                              xlim=c(-max(abs(scores[,axes])),max(abs(scores[,axes])))*1.3,
                              ylim=c(-max(abs(scores[,axes])),max(abs(scores[,axes])))*1.3,
                              asp=1, pos=3, display_names=T,cex=1,text.cex=.8,
                              ...){

  ## this will be kept simple (with intent) for quite a while


  plot(0, type="n", xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, axes=F, asp=asp)
  abline(h=0,lty=2,lwd=2, col="grey60")
  abline(v=0,lty=2,lwd=2, col="grey60")
  points(scores[,axes], col=col, pch=pch, cex=cex, ...)


  if (display_names) {
    text(scores[, axes], labels = rownames(scores),
         pos = pos, col = col, cex = text.cex)
  }

}


#' @export
ep.scree <- function(taus, main="Scree", xlab = "Components", ylab="% explained variance", lty = 1, lwd = 1, pch=20, cex=1.5, col="mediumorchid4", ...){

  plot(taus,type="l",ylab=ylab, xlab=xlab, main=main, lty=lty, lwd = lwd)
  points(1:length(taus),taus,pch=pch,col=col, cex = cex, ...)

}
