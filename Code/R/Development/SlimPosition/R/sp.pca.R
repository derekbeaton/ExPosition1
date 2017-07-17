## TODO: JRR

  ## I had a 'compact = T' here but I'm not sure what the role of compact would be anymore.
sp.pca <- function(DATA, center = T, scale = T, k = 0, graphs = F){

  res <- gsvd(expo.scale(DATA, center = center, scale = scale), k = k)


  if(graphs){

  }

}
