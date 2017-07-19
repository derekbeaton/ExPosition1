## TODO: JRR

  ## Compact will be only: fi, fj, u, v, and d.orig
sp.pca <- function(DATA, center = T, scale = T, k = 0, compact = T, graphs = F){

  res <- gsvd(expo.scale(DATA, center = center, scale = scale), k = k)


  if(graphs){

  }
  if(compact){

  }
}
