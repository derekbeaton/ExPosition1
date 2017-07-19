##mca (symmetric vs. asymmetric allowed)

sp.mca <- function(DATA, make.data.nominal = T, k = 0, compact = T, graphs = F){

  if(make.data.nominal){
    DATA <- makeNominalData(DATA)
  }
  wi <- rowSums(DATA)/sum(DATA)
  wj <- colSums(DATA)/sum(DATA)
  Ox <- DATA/sum(DATA)

  res <- gsvd( Ox - (wi %o% wj), 1/wi, 1/wj, k = k )


  if(graphs){

  }
  if(compact){

  }

}
