##mca (symmetric vs. asymmetric allowed)

sp.mca <- function(DATA, make.data.nominal = T, k = 0, compact = T, graphs = F){

  if(make.data.nominal){  ## if not I hope you know what you're doing!
    DATA <- makeNominalData(DATA)
  }
  sum.data <- sum(DATA)
  wi <- rowSums(DATA)/sum.data
  wj <- colSums(DATA)/sum.data

  res <- gsvd( (DATA/sum.data) - (wi %o% wj), 1/wi, 1/wj, k = k )

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
