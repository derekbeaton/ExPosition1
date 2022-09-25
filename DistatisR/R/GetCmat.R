#' @title GetCmat
#' @description Computes the RV coefficient matrix
#' @param CubeCP A 3D array of cross-product matrices
#' @param RV Boolean, if TRUE, GetCmat computes the matrix of the RV coefficients between all the slices of the 3D array, otherwise, GetCmat computes a scalar product. 
#' @return A matrix of either RV coefficients or scalar products.
#' @examples 
#' \dontrun{
#' D3 <- array(c(0, 1, 2, 1, 0, 1, 2, 1, 0, 
#'               0, 3, 3, 3, 0, 3, 3, 3, 0), 
#'             dim = c(3, 3, 2))
#' GetCmat(D3)             
#' }
#' @rdname GetCmat
#' @export 
GetCmat <- function(CubeCP, RV = TRUE) {
  # Compute a C matrix (or RV) from a cube of CP
  # get the dimensions
  nI <- dim(CubeCP)[1]
  nJ <- dim(CubeCP)[3]
  # reshape the 3D array as an (nI^2)*(nJ) matrix
  CP2 <-  array(CubeCP, dim = c(nI * nI, nJ))
  C <- t(CP2) %*% CP2 # Scalar Product
  if (RV) {
    # RV is TRUE we want the RV coefficient instead of the Scalar Product
    laNorm <- sqrt(apply(CP2 ^ 2, 2, sum))
    C <- C / (t(t(laNorm)) %*% laNorm)
  } # end if
  rownames(C) <- colnames(C) <- dimnames(CubeCP)[[3]]
  return(C)
}
