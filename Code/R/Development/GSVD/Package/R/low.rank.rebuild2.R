#'
#'  @export
#'
#'  @title \code{low.rank.rebuild}: raise matrix to a power and rebuild lower rank version
#'
#'  @description \code{low.rank.rebuild} takes in a matrix and will rebuild a lower rank estimate.
#'
#'  @param x data matrix
#'  @param rank.rebuild the number of components to retain in order to build a lower rank estimate of \code{x}
#'  @param ... parameters to pass through to \code{\link{tolerance.svd}}
#'
#'  @return
#'  Low rank version of \code{x}
#'
#'  @seealso \code{\link{tolerance.svd}}, \code{\link{power.rebuild_matrix}}, and \code{\link{invert.rebuild_matrix}}
#'
#'  @examples
#'
#'
#'  @author Derek Beaton
#'
#'  @keywords multivariate, diagonalization, eigen, pseudo-inverse, Moore-Penrose
#'


low.rank.rebuild <- function(x, rank.rebuild = 0, ...){

  ## quick tests for escape
  if( !is.numeric(rank.rebuild) ){
    stop("rank.rebuild is not numeric")
  }else{
    if( is.infinite(rank.rebuild) | is.nan(rank.rebuild) ){
      stop("rank.rebuild is Inf or NaN")
    }
  }

  if( length(rank.rebuild)==0 ){
    stop("rank.rebuild is of length 0")
  }


  if( length(rank.rebuild) > 1 ){

    ## this allows for a rebuild of arbitrary components...
    comps.to.rebuild <- unique( round(ifelse(rank.rebuild < 1, 1, rank.rebuild)) )
    res <- tolerance.svd(x, nu = max(comps.to.rebuild), nv = max(comps.to.rebuild), ...)
    return( sweep(res$u[,comps.to.rebuild],2,res$d[comps.to.rebuild],"*") %*% t(res$v[,comps.to.rebuild]) )


  }else { #if(length(rank.rebuild) == 1){
    if( rank.build <= 0 ){
      stop("rank.rebuild is a negative or 0 value")
    }

    ## this allows for a rebuild of some cumulative percentage
    if(rank.rebuild > 0 & rank.rebuild < 1 ){
      res <- tolerance.svd(x)
      comps.to.rebuild <- 1:(max(which( cumsum(res$tau) <= rank.rebuild))+1)
      return( sweep(res$u[,comps.to.rebuild],2,res$d[comps.to.rebuild],"*") %*% t(res$v[,comps.to.rebuild]) )

    ## this rebuilds based on first K components where K = round(rank.rebuild)
    }else {#if( (rank.rebuild > 0) ){
      res <- tolerance.svd(x, nu = round(rank.rebuild), nv = round(rank.rebuild) )
      return( sweep(res$u,2,res$d[1:round(rank.rebuild)],"*") %*% t(res$v) )

    }
  }


}
