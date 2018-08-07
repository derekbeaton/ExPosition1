sp.conditional.mca <- function(DATA, CONFOUND, make.data.nominal = T, make.confound.nominal = F, asymmetric = F, k = 0, compact = T, graphs = F){

  if(make.confound.nominal){  ## if not I hope you know what you're doing!
    CONFOUND <- makeNominalData(CONFOUND)
  }
  if(make.data.nominal){  ## if not I hope you know what you're doing!
    DATA <- makeNominalData(DATA)
  }

  	
  	## needs speed up; it's not necessarily efficient nor matched to the other methods yet.
  N <- sum(DATA)
  Or <- DATA/N
  m <- rowSums(Or)
  w <- colSums(Or)
  Er <- m %o% w 
  
  MODEL <- (CONFOUND/sum(DATA)) %*% t( t(DATA) %*% CONFOUND %*% diag(1/colSums(CONFOUND)) )   
  DATA.prime <- (Or - MODEL + Er) * N
	
  return( res <- sp.mca(DATA = DATA.prime, make.data.nominal = F, asymmetric = asymmetric, k = k, compact = compact, graphs = graphs) )
	
}