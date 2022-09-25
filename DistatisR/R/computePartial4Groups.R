# Entête ----
# function computePartial4Groups for DistatisR
# Compute 
# alpha and partial factor scores per group
# Current version November 7, 2020.
# Hervé Abdi
# ________________________________________________
#  functions in this file
# computePartial4Groups
# print.F_alpha_k


#_________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(computePartial4Groups)
#
#_________________________________________________
#' @title Computes group alphas 
#' and group factor scores
#' for \eqn{K} groups of observations 
#' in \code{distatis}.
#' @description \code{computePartial4Groups}:
#' Computes group alphas and group factor scores
#' for \eqn{K} groups of observations used to compute
#' the compromise (i.e., matrix **S**)
#' in a \code{distatis} analysis.
#' 
#' @param resDistatis The results of
#' a Distatis analysis (as performed by
#' the \code{DistatisR::distatis}) function.
#' @param DESIGN A Design vector describing
#' the groups of observations
#' @return A list with
#' \itemize{ 
#'  \item{"GroupFS: "}{
#'      The \eqn{K} weighted mean coordinates}
#'   \item{"groupAlpha: "}{
#'      The \eqn{K} alpha weights}
#'     }
#' @details In DISTATIS, the compromise
#' is computed as a weighted 
#' (with the alpha-coefficients) sum of the 
#' \eqn{K} pseudo-covariance matrices 
#' **S**k,  \code{computePartial4Groups}
#' sums all the alpha coefficients of a group
#' to compute each group partial factor scores.
#' @author Hervé Abdi
#' @seealso \code{\link{distatis}}
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname computePartial4Groups
#' @export 

# computePartial4Groups
computePartial4Groups <-  function( 
         resDistatis,  # the results of distatis
         DESIGN      # A vector indicating the groups
){
F_j     <- resDistatis$res4Splus$PartialF
alpha_j <- resDistatis$res4Cmat$alpha
groupsOfJudges  <- DESIGN
code4Groups     <- unique(groupsOfJudges)
nK <- length(code4Groups)
# initialize F_K and alpha_k
F_k <- array(0, 
             dim = c(dim(F_j)[[1]], dim(F_j)[[2]],nK))
dimnames(F_k) <- list(dimnames(F_j)[[1]], 
                      dimnames(F_j)[[2]], code4Groups)
alpha_k <- rep(0, nK)
names(alpha_k) <- code4Groups
Fa_j <- F_j
# A horrible loop
for (j in 1:dim(F_j)[[3]]){ Fa_j[,,j]  <- F_j[,,j] * alpha_j[j] }
# Another horrible loop
for (k in 1:nK){
  lindex <- groupsOfJudges == code4Groups[k]
  alpha_k[k] <- sum(alpha_j[lindex])
  F_k[,,k] <- (1/alpha_k[k])*apply(Fa_j[,,lindex],c(1,2),sum)
}

return.list <- structure(
   list(   groupFS = F_k, groupAlpha = alpha_k),
  class = 'F_alpha_k')
return(return.list)
} # End of function computePartial4Groups

#_________________________________________________
#' Print F_alpha_k results
#'
#' @param x a list that contains items for
#' the function \code{computePartial4Groups}
#'  output:
#' \code{F_alpha_k} class.
#' @param \dots inherited/passed arguments 
#' for S3 print method(s).
#' @author Hervé Abdi
#' @keywords print
#' @keywords internal
#' @export
print.F_alpha_k <-
  function (x,...) {
    ndash = 70 # How many dashes for separation lines
    cat(rep("-", ndash), sep = "")
    cat("\nGroup alpha- and partial factors-scores from distatis")
    cat("\n",rep("-", ndash), sep = "")
    cat("\n$groupFS:    ", "The Group Factor Scores")
    cat("\n$groupAlpha: ", "The Group Alpha  Values\n")
    cat(rep("-", ndash), sep = "")
    cat("\n")
    invisible(x)
  } # end of function print.F_alpha_k ----
