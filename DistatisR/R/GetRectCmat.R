#' @title GetRectCmat
#' @description Computes the rectangular RV coefficient matrix between two arrays of conformable matrices
#' @param CubeCP1 A 3D array of cross-product matrices
#' @param CubeCP2 A 3D array of cross-product matrices
#' @param RV Boolean, if TRUE, GetCmat computes the matrix of the RV coefficients between all the slices of the 3D array, otherwise, GetCmat computes a scalar product. 
#' @return A rectangular matrix of either RV coefficients or scalar products.
#' @examples 
#' \dontrun{
#' D3.1 <- array(c(0, 1, 2, 1, 0, 1, 2, 1, 0, 
#'               0, 3, 3, 3, 0, 3, 3, 3, 0), 
#'             dim = c(3, 3, 2))
#' D3.2 <- array(cc(1, 0, 0, 0, 1, 0, 0, 0, 1, 
#'                  1, 1, 1, 1, 1, 1, 1, 1, 1, 
#'                  0, 0, 0, 0, 0, 0, 0, 0, 0), 
#'             dim = c(3, 3, 3))
#' GetRectCmat(D3.1, D3.2)             
#' }
#' @rdname GetRectCmat
#' @export 
GetRectCmat <- function(CubeCP1, CubeCP2, RV = TRUE) {
  # Compute a C matrix (or RV) from a cube of CP
  # get the dimensions
  nI <- dim(CubeCP1)[1]
  nJ1 <- dim(CubeCP1)[3]
  nJ2 <- dim(CubeCP2)[3]
  if (nI != dim(CubeCP1)[2]) stop("Non conformable matrices!")
  # reshape the 3D array as an (nI^2)*(nJ) matrix
  CP1 <-  array(CubeCP1, dim = c(nI * nI, nJ1))
  CP2 <-  array(CubeCP2, dim = c(nI * nI, nJ2))
  C <- t(CP1) %*% CP2 # Scalar Product
  if (RV) {
    # RV is TRUE we want the RV coefficient instead of the Scalar Product
    laNorm1 <- sqrt(apply(CP1 ^ 2, 2, sum))
    laNorm2 <- sqrt(apply(CP2 ^ 2, 2, sum))
    C <- C / (t(t(laNorm1)) %*% laNorm2)
  } # end if
  rownames(C) <- dimnames(CubeCP1)[[3]]
  colnames(C) <- dimnames(CubeCP2)[[3]]
  return(C)
}
