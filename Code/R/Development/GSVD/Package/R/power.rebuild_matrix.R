#'
#'  @export
#'
#'  @title \code{power.rebuild_matrix}: raise matrix to a power and rebuild lower rank version
#'
#'  @description \code{power.rebuild_matrix} takes in a matrix and will compute raise that matrix to some arbitrary power via the singular value decomposition.
#'  Additionally, the matrix can be computed for a lower rank estimate of the matrix.
#'
#'  @param x data matrix
#'  @param power the power to raise \code{x} by (e.g., 2 is squared)
#'  @param k the number of components to retain in order to build a lower rank estimate of \code{x}
#'  @param ... parameters to pass through to \code{\link{tolerance.svd}}
#'
#'  @return
#'  The (possibly lower rank) raised to an arbitrary \code{power} version of \code{x}
#'
#'  @seealso \code{\link{tolerance.svd}} and \code{\link{invert.rebuild_matrix}}
#'
#'  @examples
#'  hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
#'  X <- hilbert(9)[, 1:6]
#'  X.power_1 <- power.rebuild_matrix(X)
#'  X / X.power_1
#'
#'  ## other examples.
#'  X.power_2 <- power.rebuild_matrix(X,power=2)
#'  X.power_negative.1.div.2 <- power.rebuild_matrix(X,power=-1/2)
#'
#'  X.power_negative.1 <- power.rebuild_matrix(X,power=-1)
#'  X.power_negative.1 / t(invert.rebuild_matrix(X))
#'
#'  @author Derek Beaton
#'
#'  @keywords multivariate, diagonalization, eigen, pseudo-inverse, Moore-Penrose
#'

power.rebuild_matrix <- function(x, power = 1, k=0, ...){

  ## actually, these should test if they are a vector and just return them as is.
  ## also test if diagonal, and if so, only return a vector.
  ## but maybe that's for later...

  ##stolen from MASS::ginv()
  if (length(dim(x)) > 2L || !(is.numeric(x) || is.complex(x)))
    stop("power.rebuild_matrix: 'x' must be a numeric or complex matrix")
  if (!is.matrix(x))
    x <- as.matrix(x)

  k <- round(k)
  if(k<=0){
    k <- min(nrow(x),ncol(x))
  }

    ## should be tested for speed.

  #res <- tolerance.svd(x,...)
  #comp.ret <- 1:min(length(res$d),k)
  #return( (res$u[,comp.ret] * matrix(res$d[comp.ret]^power,nrow(res$u[,comp.ret]),ncol(res$u[,comp.ret]),byrow=T)) %*% t(res$v[,comp.ret]) )

  res <- tolerance.svd(x, nu = k, nv = k, ...)
  return( sweep(res$u,2,res$d[1:k]^power,"*") %*% t(res$v) )

}
