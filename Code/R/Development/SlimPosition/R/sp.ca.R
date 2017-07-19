## plain ca (symmetric vs. asymmetric allowed)

  ## it's just CA, that's it.
sp.ca <- function(DATA, k = 0, compact = T, graphs = F){

  wi <- rowSums(DATA)/sum(DATA)
  wj <- colSums(DATA)/sum(DATA)
  Ox <- DATA/sum(DATA)

  res <- gsvd( Ox - (wi %o% wj), 1/wi, 1/wj, k = k )


  if(graphs){

  }
  if(compact){

  }

}
