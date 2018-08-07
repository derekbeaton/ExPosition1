  ## Compact will be only: fi, fj, u, v, and d.orig
sp.pca <- function(DATA, center = T, scale = "SS1", k = 0, compact = T, graphs = F, tol = .Machine$double.eps){

    ## no messing around with negatives and a bunch of stupid values!
  k <- ceiling(abs(k))

  res <- gsvd(expo.scale(DATA, center = center, scale = scale), k = k, tol = tol)

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
