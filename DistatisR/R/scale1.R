# A scale function that can cope with all numbers in a vector 
# being equal (all numbers are then set to 0).
# 

#'@title 
#' A variation over the base \code{R} scale function 
#' that avoids the "divide by 0 = NA"  problem.
#' 
#'@description \code{scale1}: A variation over the base 
#'\code{R} \code{scale} function.
#'  The function \code{scale1}: 
#'  centers (if needed) and scales a vector
#'  to norm 1; if the vector contains values all equal to a
#'  constant, \code{scale1} sets all values to 0
#'  (in lieu of NA as \code{scale} does). 
#'  Usefull when pre-processing tables
#'  for PCA-like type of analyses.
#'  
#'@param x a vector to be scaled
#'@param scale (default = \code{TRUE}), when \code{TRUE}
#'scale the vector to norm 1,
#'  otherwise do nothing.
#'@param center (default = \code{TRUE}), when \code{TRUE}
#'center the vectors (i.e., substract the mean from all numbers),
#'  otherwise do nothing.
#'@return a centered (if required) and norm-1 
#' (if required) normalized
#'  vector.
#'@author Hervé Abdi
#'@examples
#'  toto   <- runif(10)     # 10 random numbers between 0 and 1
#'  tutu   <- scale1(toto)  # toto centered and normalized
#'  toto0  <- rep(1,10)     # 10 numbers all equal to 1
#'  tutu0  <- scale1(toto0) # scaled to 0 # Compare with
#'  tutuNA <- scale(toto0)  # all numbers set to NA
#'
#'@rdname scale1  
#'@export

scale1 <- function(x, scale = TRUE, center = TRUE){
  # internal function
  # Uses near form dplyr but copy it to avoid loading dplyr
  near <- function(x, y, 
            tol = .Machine$double.eps^0.5) {abs(x - y) < tol}
  if (center) {z   <- x - mean(x)} else {z = x}
  if (scale){ # a horrible loop
  normx <- sqrt(sum(z^2))
  if (!near(normx, 0) ){ z <- z / normx} else {z = z * 0}
  }
  return(z)
}