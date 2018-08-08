## note to self: forget about the normalization schemes. just deal with the typical ones for now and grow later.
  ## retain the attributes() on return.


#' @export
ep.pca <- function(DATA, col.scale.type="ss1", k = 0, compact = T, graphs = F, tol = .Machine$double.eps){

  if(any(is.na(DATA))){
    stop("No NAs allowed.")
  }
  if( is.null(col.scale.type) ){
    col.scale.type <- "none"
  }
  if( !(col.scale.type %in% c("center","z","ss1","none")) ){
    stop("Only 'center', 'z', 'ss1', or 'none' scaling type allowed. If other types are needed then transform the data before analysis and use 'none'.")
  }


  k <- ceiling(abs(k))
  DATA <- col.scale(DATA, type = col.scale.type)
  res <- gsvd(DATA, k = k, tol = tol)


  res$compact <- F
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, tau = res$tau, d.orig=res$d.orig, u=res$u, v=res$v)
    res$compact <- T
  }
  res$data.attributes <- attributes(DATA) ## maybe it's none?
  res$analysis <- "pca"
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
