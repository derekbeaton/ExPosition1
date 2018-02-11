#bada
#make design nominal feature? User resonsible for making nominal?

sp.bada <- function(DATA, center = T, scale = T, DESIGN, k = 0, compact = T, graphs = F){

  DATA <- expo.scale(DATA, center = center, scale = scale)

  if(length(DESIGN)!=nrow(DATA)){
    stop("DESIGN is not a vector of length nrow(DATA). A row design is required.")
  }
  DESIGN <- makeNominalData(as.matrix(DESIGN))
  if( any(colSums(DESIGN)<2) ){
    stop("A group size of 1 has been found. An observation cannot be the only observation in a group.")
  }

  DESIGN.labs<-colnames(DESIGN)
  #DESIGN <- t(apply(DESIGN, 1, "/", colSums(DESIGN)))
  DESIGN <- apply(DESIGN,2,function(x){x/sum(x)})
  colnames(DESIGN)<-DESIGN.labs


  res <- gsvd( t(DESIGN) %*% DATA, k = k)

  res$lx <- (DATA %*% res$fj) * matrix(1/res$d,nrow(DATA),ncol(res$fj),byrow=T)
  res$ly <- (DESIGN %*% res$fi) * matrix(1/res$d,nrow(DESIGN),ncol(res$fi),byrow=T)

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
