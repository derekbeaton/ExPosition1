#' @export
sp.mca <- function(DATA, make.data.nominal = T, k = 0, compact = T, graphs = F, tol=.Machine$double.eps){

  k <- ceiling(abs(k))

  if(make.data.nominal){  ## if not I hope you know what you're doing!
    DATA <- make.data.nominal(DATA)
  }

  # ship off to CA.
  res <- sp.ca(DATA,asymmetric=asymmetric, k=k, compact=F, graphs=F, tol=tol)


  if(graphs){
    sp.component_plot(res$fi)
    sp.component_plot(res$fj)
    sp.scree(res$d.orig^2)
  }
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, d.orig=res$d.orig, u=res$u, v=res$v)
  }
  return(res)
}
