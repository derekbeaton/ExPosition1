#' @title Dist2CP
#' @description Transforms a cube of distance matrices 
#' into a cube of cross-product matrices.
#' @param D3 the cube of distance matrices.
#' @return the cube of cross-product matrices.
#' @rdname Dist2CP
#' @export
#'
#' @examples
#' \dontrun{
#' D3 <- array(c(0, 1, 2, 1, 0, 1, 2, 1, 0, 
#'               0, 3, 3, 3, 0, 3, 3, 3, 0), 
#'             dim = c(3, 3, 2))
#' Dist2CP(D3)
#' }
#' @export
Dist2CP <- function(D3) {
    CP3 <-
      (array(apply(D3, 3, DblCenterDist), dim = c(dim(D3)[1], dim(D3)[2], dim(D3)[3])))
    dimnames(CP3) <- dimnames(D3)
    return(CP3)
} 
