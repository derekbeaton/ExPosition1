## plsc

sp.plsc <- function(X, Y, center.X = T, scale.X = "SS1", center.Y = T, scale.Y = "SS1", k = 0, compact = T, graphs = F){

  if (nrow(X) != nrow(Y)) {
    stop("X and Y must have the same number of rows.")
  }

  X <- expo.scale(X, scale = scale.X, center = center.X)
  Y <- expo.scale(Y, scale = scale.Y, center = center.Y)

  res <- gsvd(t(X) %*% Y, k=k)
  res$lx <- (X %*% res$fi) * matrix(1/res$d,nrow(X),ncol(res$fi),byrow=T)
  res$ly <- (Y %*% res$fj) * matrix(1/res$d,nrow(Y),ncol(res$fj),byrow=T)

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
