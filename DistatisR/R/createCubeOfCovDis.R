# functions in this file: 
# createCubeOfCovDis
# print.cubeOfCovDis
#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(createCubeOfCovDis)
#
#_____________________________________________________________________

#_____________________________________________________________________
# function createCubeOfCovDis 
#

#' @title compute a cube of covariance and a cube of distance
#' between the items (rows) of a brick of measurements 
#' (when all blocks
#' have the same number of variables).
#' 
#' @description \code{createCubeOfCovDis}
#' compute a cube of covariance and a cube of 
#' (squared) Euclidean distance
#' between the items (rows) of a brick of measurements.
#' The variables describing the items can scaled to norm 1
#' and centered. The whole matrix 
#' can  be scaled by its first eigenvalue
#' (a la DISTATIS). All "slices" of the brick
#' should have the same number of variables.
#' For different number of variables per block,
#' see \code{list2CubeOfCov}.
#' 
#' @details  
#' The input of  \code{createCubeOfCovDis} is a 
#' \eqn{I} items by \eqn{J} quantitative variables
#' by \eqn{K} assessors (as obtained, e.g., from a projective
#' mapping task).
#' 
#' By default  \code{createCubeOfCovDis}
#' centers and normalizes each column for each slice of the brick
#' and then normalize each covariance matrix such that
#' the first eigenvalue of each covariance matrix is equal to 1.
#'  
#'A \code{distatis} analysis of the Distance matrices with
#' the option \code{Distance = TRUE} will give the same results
#' as the \code{distatis} analysis of the Covariance matrices with
#' the option \code{Distance = FALSE}.
#'    
#' @param brickOfData  a 
#' \eqn{I} items by \eqn{J} quantitative variables
#'  by \eqn{K} assessors.
#' @param scale (Default: \code{TRUE}), when \code{TRUE}
#' scale to norm 1 each column for each slice.
#' @param center (Default: \code{TRUE}), when \code{TRUE}
#' centers each column.
#' @param ev.scale (Default: \code{TRUE}), when \code{TRUE}
#' normalizes each slice 
#' (i.e., each \eqn{I} items by \eqn{J}  matrix) so that its first 
#' eigenvalue is equal to 1.
#' @return a list with 1) \code{cubeOfCovariance}
#' a cube of \eqn{K}  \eqn{I} by \eqn{I} covariance matrices;
#' and 2) code{cubeOfDistance}
#' a cube of \eqn{K}  \eqn{I} by \eqn{I} 
#' (squared) Euclidean distance
#'   matrices. 
#' @examples 
#' \dontrun{
#' # use the data from the BeersProjectiveMapping dataset
#' data("BeersProjectiveMapping") 
#' # Create the I*J_k*K brick of data
#' zeBrickOfData <- projMap2Cube(
#'                      BeersProjectiveMapping$ProjectiveMapping, 
#'                      shape = 'flat',  nVars = 2)
#' # Create the cubes of Covariance and Distance                     
#' cubes <- createCubeOfCovDis(zeBrickOfData$cubeOfData)
#' }
#' @seealso list2CubeOfCov
#' @author Herve Abdi
#' @rdname createCubeOfCovDis
#' @export 

createCubeOfCovDis <- function(brickOfData, 
                               scale    = TRUE,
                               center   = TRUE,
                               ev.scale = TRUE){
  if (length(dim(brickOfData)) != 3){
    stop('brickOfData needs to be a 3-way array')
  } # end if 
  nI = dim(brickOfData)[[1]]
  # nJ = dim(brickOfData)[[2]]
  nK = dim(brickOfData)[[3]]
  cubeOfCov <- array(NA, dim = c(nI,nI,nK))
  cubeOfDis <- array(NA, dim = c(nI,nI,nK))
  # First compute the covariance matrices
  for (k in 1:nK){
    # step one normalize per column
    norm_Xk <- apply( brickOfData[,,k],2,scale1,scale,center)
    cov_k   <- norm_Xk %*% t(norm_Xk)
    if (ev.scale) {
      cov_k <- cov_k / eigen(cov_k, 
                             symmetric = TRUE,
                             only.values = TRUE)$values[1]
    } # end if
    cubeOfCov[,,k] <- cov_k
    d_k <- matrix(diag(cov_k), nrow = nI, ncol = nI, byrow = TRUE) +
      matrix(diag(cov_k), nrow = nI, ncol = nI, byrow = FALSE) -
      2*cov_k
    cubeOfDis[,,k] <- d_k 
    # test here
  } # end loop on k
  dimnames(cubeOfCov)[[1]]  <- dimnames(brickOfData)[[1]]
  dimnames(cubeOfCov)[[2]]  <- dimnames(brickOfData)[[1]]
  dimnames(cubeOfCov)[[3]]       <- dimnames(brickOfData)[[3]]
  dimnames(cubeOfDis)[[1]]  <- dimnames(brickOfData)[[1]]
  dimnames(cubeOfDis)[[2]]  <- dimnames(brickOfData)[[1]]
  dimnames(cubeOfDis)[[3]]       <- dimnames(brickOfData)[[3]]
  return.list <- structure(list(cubeOfCovariance = cubeOfCov,
                                cubeOfDistance   = cubeOfDis),
                           class = 'cubeOfCovDis')
  return(return.list)
} # end of function createCubeOfCovDis

#_____________________________________________________________________
# The print function

#_____________________________________________________________________

#_____________________________________________________________________
# Print function for cubeOfCovDis
#*********************************************************************
#_____________________________________________________________________
#' Change the print function for the class \code{cubeOfCovDis}
#'
#' Change the print function for the class \code{cubeOfCovDis}: 
#' (output of \code{createCubeOfCovDis}.
#'
#' @param x a list objects of the class {cubeOfCovDis}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
print.cubeOfCovDis <- function(x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 1 Cube of Covariance and 1 Cube of Squared Euclidean Distance. \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$cubeOfCovariance ", "An I*I*K cube of I*I covariance matrices.")
  cat("\n$cubeOfDistance   ", "An I*I*K cube of I*I distance matrices.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.cubeOfCovDis
#_____________________________________________________________________
