#' @title ComputeSplus
#' @description Compute the compromise matrix for STATIS/DISTATIS
#' @param CubeCP A 3D array of cross-product matrices.
#' @param alpha The vector of weights
#' @return The compromise matrix
#' computed as the alpha-weighted sum of the 
#' cross-product matrices.
#'  
#' @examples 
#' \dontrun{
#' D3 <- array(c(0, 1, 2, 1, 0, 1, 2, 1, 0, 
#'               0, 3, 3, 3, 0, 3, 3, 3, 0), 
#'             dim = c(3, 3, 2))
#' ComputeSplus(D3, alpha = c(1, 0.5))
#' }
#' @rdname ComputeSplus
#' @export 
ComputeSplus <- function(CubeCP, alpha) {
  # 
  nI <- dim(CubeCP)[1]
  nJ <- dim(CubeCP)[3]
  Splus <- matrix(0, nI, nI)
  # Formerly a Horrible Loop. Changed in final version
  # for (i in 1:nJ){ Splus = Splus + alpha[i]*CubeCP[,,i] }
  # A better way
  Splus <- apply(apply(CubeCP, c(1, 2), '*', t(alpha)), c(2, 3), sum)
  return(Splus)
} # end ComputeSplus
