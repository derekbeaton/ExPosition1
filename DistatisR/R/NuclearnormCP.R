# sinew::makeOxygen(NuclearNormedCP)
#' @title NuclearNormedCP
#' @description Normalizes a positive 
#' semi-definite matrix by diving it by its
#' nuclear norm (i.e., the sum of the square root of
#' its eigen-values).
#' @param Y The matrix to normalize
#' @return The normalized matrix 
#' @examples 
#' \dontrun{
#' A <- toeplitz(c(1, 0.6))
#' NuclearNormedCP(A)
#' }
#' @rdname NuclearNormedCP
#' @export 
NuclearNormedCP <- function(Y) {
    # Nuclear Normed Normalize a psd matrix (i.e., first eigenvalue=1)
    ev = eigen(Y, symmetric = T, only.values = T)[1]
    goodEig <- ev$values[ev$values > 2*.Machine$double.eps]
    eNu = sum(goodEig^(1/2))
    Ynormed = Y / eNu
    return(Ynormed)
  } # End of NuclearNormCP
