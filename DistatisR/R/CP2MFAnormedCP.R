#' @title CP2MFAnormedCP
#' @description MFA normalizes a cube of cross-product matrices
#' @param CP3 A 3D array of cross-product matrices
#' @return The 3D array of the normalized cross-product matrices.
#' @examples 
#' \dontrun{
#' D3 <- array(c(0, 1, 2, 1, 0, 1, 2, 1, 0,
#'               0, 3, 3, 3, 0, 3, 3, 3, 0), 
#'            dim = c(3, 3, 2))
#' CP2MFAnormedCP(D3)         
#' }
#' 
#' @rdname CP2MFAnormedCP
#' @export 
CP2MFAnormedCP <-
  function(CP3) {
    # Transform a cube of CP into an MFA normed cube of CP
    CP3normed <- array(apply(CP3, 3, MFAnormCP), dim = dim(CP3))
    dimnames(CP3normed) <- dimnames(CP3)
    return(CP3normed)
  }
