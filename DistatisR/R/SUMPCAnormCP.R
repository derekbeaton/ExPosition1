#' @title SUMPCAnormCP
#' @description Normalizes a positive semi-definite matrix (i.e., total intertia=1)
#' @param Y Matrix to normalize
#' @return Normalized matrix
#' @examples 
#' \dontrun{
#' A <- toeplitz(c(1, 0.6))
#' SUMPCAnormCP(A)
#' }
#' @rdname SUMPCAnormCP
#' @export 
SUMPCAnormCP <- function(Y) {
    Ynormed <- Y / sum(diag(Y))
    return(Ynormed)
} 
