## plain ca (symmetric vs. asymmetric allowed)
## it's just CA, that's it.
sp.ca <- function(DATA, asymmetric = F, k = 0, compact = T, graphs = F){

  sum.data <- sum(DATA)
  rowSums.data <- rowSums(DATA)
  wi <- rowSums.data/sum.data
  wj <- colSums(DATA)/sum.data

    ## big question: which is faster:

    ## (1)
  #res <- gsvd( (DATA/sum.data) - (wi %o% wj), 1/wi, 1/wj, k = k )
    ## (1)

    ## (2)
  res <- gsvd( sweep(sweep(DATA,1,rowSums.data,"/"),2,wj), wi, 1/wj, k = k )
  res$fi <- sweep(res$fi,1,wi,"/")
    ## (2)


  if(asymmetric){
    res$fj <- sweep(res$fj,2,res$d,"/")
  }
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
