## note to self: forget about the normalization schemes. just deal with the typical ones for now and grow later.
  ## retain the attributes() on return.


#' @export
ep.pca <- function(DATA, center = T, scale = "SS1", k = 0, compact = T, graphs = F, tol = .Machine$double.eps){

  if(any(is.na(DATA))){
    stop("No NAs allowed.")
  }
  k <- ceiling(abs(k))

  ## as for data, there's only so much I can know and record
    ### if you get weird, you get weird. I can't bootstrap weird.

    ## REALIZATION: most of expo.scale doesn't actually matter...
  DATA <- scale(DATA, center = center, scale = scale)
  res <- gsvd(DATA, k = k, tol = tol)

  if(graphs){
    ep.component.plot(res$fi)
    ep.component.plot(res$fj)
    ep.scree(res$d.orig^2)
  }
  if(compact){
    res <- list(fi=res$fi, fj=res$fj, d.orig=res$d.orig, u=res$u, v=res$v)
  }

  res$data.attributes <- attributes(DATA) ## maybe it's none?
  return(res)
}
