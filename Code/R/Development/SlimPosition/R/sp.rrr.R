### rrr/rda

sp.rrr <- function(X, Y, center.X = T, scale.X = T, center.Y = T, scale.Y = T, k = 0, compact = T, graphs = F){

  X <- expo.scale(X, center = center.X, scale = scale.X)
  Y <- expo.scale(Y, center = center.Y, scale = scale.Y)

  res <- gsvd(
    t(X) %*% Y, crossprod(X),  k = k
  )
  LX <- X %*% res$fi %*% diag(1/res$d)
  LY <- Y %*% res$fj %*% diag(1/res$d)

  if(graphs){

  }
  if(compact){

  }
}
