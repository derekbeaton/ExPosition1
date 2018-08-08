#' @export
benzecri.correction <- function(res, num.variables){

  # check res type to make sure it's either MCA or CA or GSVD (refuse otherwise)
    ## will implement that a bit later.

  ## make this a function so it can be called post hoc...
  keepers <- which(res$d.orig^2 > (1/num.variables))

  new.eigvals <- ((num.variables/(num.variables - 1)) * ( (res$d.orig[keepers]^2) - (1/num.variables)))^2
  res$d.orig <- sqrt(new.eigvals)
  res$tau <- (new.eigvals / sum(new.eigvals)) * 100


  k.length <- length(res$d)
  this.k <- min(k.length,length(keepers))

  scale.factors <- res$d.orig[1:this.k] / res$d[1:this.k]

  res$fi <- res$fi[,1:this.k]
  res$fj <- res$fj[,1:this.k]

  res$fi <- sweep(res$fi[,1:this.k], 2, scale.factors, "*")
  res$fj <- sweep(res$fj[,1:this.k], 2, scale.factors, "*")
  res$u <- res$u[,this.k]
  res$v <- res$v[,this.k]
  res$d <- res$d[this.k]

  if("p" %in% names(res)){
    res$p <- res$p[,this.k]
  }
  if("q" %in% names(res)){
    res$q <- res$q[,this.k]
  }

  return(res)

}
