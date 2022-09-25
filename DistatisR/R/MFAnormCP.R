# sinew::makeOxygen(MFAnormCP)
#' @title MFAnormCP
#' @description Normalizeq a positive semi-definite matrix matrix product such that its first eigenvalue is equal to one
#' @param Y The matrix to normalize
#' @return The normalized matrix 
#' @examples 
#' \dontrun{
#' A <- toeplitz(c(1, 0.6))
#' MFAnormCP(A)
#' }
#' @rdname MFAnormCP
#' @export 
MFAnormCP <- function(Y) {
    # MFA Normalize a psd matrix (i.e., first eigenvalue=1)
    ev = eigen(Y, symmetric = T, only.values = T)[1]
    e1 = ev$values[1]
    Ynormed = Y / e1
    return(Ynormed)
  } # End of MFAnormCP
