#dica

sp.dica <- function(DATA, make.data.nominal = T, DESIGN, k = 0, compact = T, graphs = F){

  if(length(DESIGN)!=nrow(DATA)){
    stop("DESIGN is not a vector of length nrow(DATA). A row design is required.")
  }
  DESIGN <- makeNominalData(as.matrix(DESIGN))
  if( any(colSums(DESIGN)<2) ){
    stop("A group size of 1 has been found. An observation cannot be the only observation in a group.")
  }

  R <- t(as.matrix(DESIGN)) %*% as.matrix(DATA)
  sum.R <- sum(R)
  wi <- rowSums(R)/sum.R
  wj <- colSums(R)/sum.R

  res <- gsvd( (R/sum.R) - (wi %o% wj), 1/wi, 1/wj, k = k )
  rm(R); gc() ## clean up.

  res$lx <- rowNorms(DATA,type="ca") %*% res$fj * matrix(1/res$d,nrow(fj),ncol(fj),byrow=T)
  res$ly <- DESIGN %*% res$fi * matrix(1/res$d,nrow(fi),ncol(fi),byrow=T)

    ### we may want to consider a different plot for LVs here because $ly is just fi, e.g.,
  # sp.component_plot(res$lx)
  if(graphs){
    sp.component_plot(res$fi)
    sp.component_plot(res$fj)
    sp.latentvar_plot(res)
    sp.latentvar_plot(res,axis=2)
    sp.scree(res$d.orig^2)
  }
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, d.orig=res$d.orig, u=res$u, v=res$v, lx=res$lx)
  }

  return(res)
}
