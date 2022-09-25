# sinew::makeOxygen(ldiag)
# rdiag & ldiag ----
# Two other local functions ----------------------------------------
# to replace left and right diag multiplication
#' @title  Left (i.e., pre) Multiply a matrix by a diagonal matrix
#'
#' @description  \code{ldiag}: Left (i.e., pre) Multiply
#' a matrix by a diagonal matrix (with only
#'  the diagonal elements being given).
#' @param y a \eqn{I} element
#' vector (of the diagonal elements of an \eqn{I} by \eqn{I} matrix)
#' @param X an \eqn{I} by \eqn{J} matrix.
#' @return an \eqn{I} by \eqn{J} matrix equal
#' to diag(\strong{y}) %*% \strong{X}.
#' @author Hervé Abdi
#' @rdname ldiag
#' @seealso \code{\link{rdiag}}
#' @examples
#' \dontrun{
#'  Z <- ldiag(y, X)
#'  }
#' @keywords internal
ldiag <- function(y,X){
  nR <- length(y)
  nC <- ncol(X)
  return(matrix(y, nrow = nR, ncol = nC, byrow = FALSE) * X)
}
#_____________________________________________________________________
# rdiag preamble ----
#'@title  right (i.e., post) Multiply a matrix by a diagonal matrix
#'
#' @description \code{rdiag}: right (i.e., post) Multiply
#' a matrix by a diagonal matrix (with only
#'  the diagonal elements being given).
#' @param y a \eqn{J} element
#' vector (of the diagonal elements of a \eqn{J} by \eqn{J} matrix)
#' @param X an \eqn{I} by \eqn{J} matrix.
#' @return an \eqn{I} by \eqn{J} matrix equal to
#' \strong{X} %*% diag(\strong{y}).
#' @author Hervé Abdi
#' @seealso \code{\link{ldiag}}
#' @rdname rdiag
#' @examples
#' \dontrun{
#'  Z = rdiag(X,y)
#'  }
#' @keywords internal
rdiag <- function(X,y){
  nC <- length(y)
  nR <- nrow(X)
  return(X * matrix(y, nrow = nR, ncol = nC, byrow = TRUE))
}
# __________________________________________________________________
