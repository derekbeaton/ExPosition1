### rrr/rda

sp.rrr <- function(X, Y, center.X = T, scale.X = T, center.Y = T, scale.Y = T, k = 0, compact = T, graphs = F){

  X <- expo.scale(X, center = center.X, scale = scale.X)
  Y <- expo.scale(Y, center = center.Y, scale = scale.Y)

  res <- gsvd(
    t(X) %*% Y, crossprod(X),  k = k
  )
    ## can be more efficient with the d multiply.
  res$lx <- X %*% res$fi %*% diag(1/res$d)
  res$ly <- Y %*% res$fj %*% diag(1/res$d)

  if(graphs){
    sp.component_plot(res$fi)
    sp.component_plot(res$fj)
    ## just make a separate "sp.latentvar_plot() specifically for latent variables
    #sp.component_plot( cbind(res$lx[,1], res$ly[,1] ), main="Latent variable 1", xlab="LX 1", ylab="LY 1", asp=NA)
    #sp.component_plot( cbind(res$lx[,2], res$ly[,2] ), main="Latent variable 2", xlab="LX 2", ylab="LY 2", asp=NA)
    sp.scree(res$d.orig^2)
  }
  if(compact){

  }

  return(res)
}
