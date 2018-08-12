sp.conditional.ca <- function(DATA, CONFOUND, make.confound.nominal = F, asymmetric = F, k = 0, compact = T, graphs = F){

  if(make.confound.nominal){  ## if not I hope you know what you're doing!
    CONFOUND <- makeNominalData(CONFOUND)
  }
  ## I have to test the confound here.

  	## needs speed up; it's not necessarily efficient nor matched to the other methods yet.
  N <- sum(DATA)
  Or <- DATA/N
  m <- rowSums(Or)
  w <- colSums(Or)
  Er <- m %o% w

  MODEL <- (CONFOUND/sum(DATA)) %*% t( t(DATA) %*% CONFOUND %*% diag(1/colSums(CONFOUND)) )
  DATA.prime <- (Or - MODEL + Er) * N

  return( res <- sp.ca(DATA = DATA.prime, asymmetric = asymmetric, k = k, compact = compact, graphs = graphs) )

}
