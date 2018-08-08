#' @export
sp.ca <- function(DATA, asymmetric = F, k = 0, compact = T, graphs = F, tol = .Machine$double.eps){

  if(any(is.na(DATA))){
    stop("No NAs allowed.")
  }


  sum.data <- sum(DATA)
  wi <- rowSums(DATA)/sum.data
  wj <- colSums(DATA)/sum.data

  k <- ceiling(abs(k))

    ## big question: which is faster:
    ## (1)
  #res <- gsvd( (DATA/sum.data) - (wi %o% wj), 1/wi, 1/wj, k = k )
    ## (1)
    ## (2)
  #res <- gsvd( sweep(sweep(DATA,1,rowSums.data,"/"),2,wj), wi, 1/wj, k = k , tol = tol)
  res <- gsvd( sweep(sweep(DATA,1,wi,"*"),2,wj), wi, 1/wj, k = k , tol = tol)
  res$fi <- sweep(res$fi,1,wi,"/")
    ## (2)

  res$asymmetric <- asymmetric
  if(asymmetric){
    res$fj <- sweep(res$fj,2,res$d,"/")
  }
  res$compact <- F
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, tau = res$tau, d.orig=res$d.orig, u=res$u, v=res$v)
  }
  res$data.attributes <- attributes(DATA) ## maybe it's none?
  res$analysis <- "ca"
  class(res) <- "expo"


  if(graphs){
    max.lims <- apply(rbind(res$fi[,1:2], res$fj[,1:2]),2,max) * 1.25
    min.lims <- apply(rbind(res$fi[,1:2], res$fj[,1:2]),2,min) * 1.25
    xlims <- c(min.lims[1],max.lims[1])
    ylims <- c(min.lims[2],max.lims[2])
    c1.label <- paste0("Component 1: ", round(res$tau[1],digits=3), " % variance" )
    c2.label <- paste0("Component 2: ", round(res$tau[2],digits=3), " % variance" )
    plot(res,type="row.scores", main="Row component scores", xlim = xlims, ylim = ylims, xlab= c1.label, ylab = c2.label)
    plot(res,type="col.scores", main="Column component scores", xlim = xlims, ylim = ylims, xlab= c1.label, ylab = c2.label)
    plot(res,type="scree")
  }

  return(res)
}
