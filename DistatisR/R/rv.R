#' Function to compute the RV coefficient between to conformable matrices
#'
#' @param A a matrix,
#' @param B a second matrix.
#'
#' @return the RV coefficient between A and B.
#' @export
#'
#' @examples
#' 
#' A <- toeplitz(2:1)
#' B <- diag(2)
#' rv(A, B)
#' @export
rv <- function (A, B) {
  ab <- base::sum(A * B)
  aa <- base::sum(A * A)
  bb <- base::sum(B * B)
  return(ab / base::sqrt(aa * bb))
}
