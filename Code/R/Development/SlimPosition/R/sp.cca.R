### cca

sp.cca <- function(X, Y, center.X = T, scale.X = T, center.Y = T, scale.Y = T, k = 0, compact = T, graphs = F){

  X <- expo.scale(X, center = center.X, scale = scale.X)
  Y <- expo.scale(Y, center = center.Y, scale = scale.Y)

  res <- gsvd(
    t(X) %*% Y, crossprod(X), crossprod(Y),  k = k
  )
    ## can be more efficient with the d multiply.
  res$lx <- X %*% res$fi %*% diag(1/res$d)
  res$ly <- Y %*% res$fj %*% diag(1/res$d)

  if(graphs){
    sp.component_plot(res$fi)
    sp.component_plot(res$fj)
    sp.latentvar_plot(res)
    sp.latentvar_plot(res,axis=2)
    sp.scree(res$d.orig^2)
  }
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, d.orig=res$d.orig, u=res$u, v=res$v, lx=res$lx, ly=res$ly)
  }

  return(res)
}
