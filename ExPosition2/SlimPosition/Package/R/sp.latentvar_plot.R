sp.latentvar_plot <- function(res, axis=1, pch=20, col="mediumorchid4", line.col="grey80", lty=2, lwd=2,
                              main="Latent Variables",
                              pos=3, display_names=T,
                              ...){

  these.points <- cbind(res$lx[,axis],res$ly[,axis])
    rownames(these.points) <- rownames(res$lx)
  plot(these.points, type="n", main=main, xlab=paste0("LX ",axis), ylab=paste0("LY ",axis), axes=F)
  abline(h=0,lty=2,lwd=2, col="grey60")
  abline(v=0,lty=2,lwd=2, col="grey60")
  points(these.points, col=col, pch=pch, ...)
  ## will try to employ a "repel" later.
  if(display_names){
    text(these.points,labels=rownames(these.points),pos=pos,col=col)
  }


}
