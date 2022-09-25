#' Double Center a distance matrix
#'
#' @param Y a "distance" matrix,
#'
#' @return a cross-product matrix (if the distance is Euclidean)
#' @export
#'
#' @examples
#' Y <- toeplitz(c(0, 3, 3))
#' DblCenterDist(Y)
#' @export
DblCenterDist <- function(Y) {
  nI <- nrow(Y)
  CentMat <- diag(nI) - (1 / nI) * matrix(1, nI, nI)
  S <- -(1 / 2) * (CentMat %*% Y %*% CentMat)
  return(S)
} # end of DblCenterDist
