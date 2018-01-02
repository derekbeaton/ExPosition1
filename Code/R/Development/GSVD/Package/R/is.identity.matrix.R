#'
#'  @export
#'
#'  @title \code{is.identity.matrix}: test if a matrix is an identity matrix
#'
#'  @description \code{is.identity.matrix} takes a matrix and tests if it is an identity matrix
#'
#'  @param x a matrix to test
#'  @param tol tolerance precision to eliminate all abs(x) values below \code{tol}. Default is .Machine$double.eps
#'
#'  @return
#'  boolean. TRUE if the matrix is an identity matrix, FALSE if the matrix is not.



is.identity.matrix <- function(x,tol=.Machine$double.eps){

  if(is.null(dim(x))){
    stop("is.identity.matrix: x is not a matrix.")
  }

  x <- as.matrix(x)
  x[abs(x) < tol] <- 0

  if(is.diagonal.matrix(x)){
    if( all(diag(x)==1) ){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }else{
    return(FALSE)
  }

}
