#  supplementalProjection4distatis ----
# Supplementary projections for distatis.
# Created by Vincent Guillemot 12/08/2019
# Modified by Hervé Abdi December 10, 2019
# Needs to be tested ----
# last fix HA: replace p (undefined) by P.

#_____________________________________________________________________
# Preamble -----
#' @title Supplementary element(s) projection in DISTATIS
#'
#' @description  \code{supplementalProjection4distatis}:
#' Computes for \code{distatis} 
#' the projection as supplementary element(s)
#' (a.k.a. "out of sample")
#' of a set of squared matrices.
#'  The matrices to be projected need to be
#' of the same type (e.g., distance, correlation) as the matrices 
#' used inn the original call to \code{distatis}.
#' 
#' @param res.distatis the results of the function \code{distatis}
#' @param elsupp the supplementary elements (i.e., a 3D array).
#'
#' @return the coordinates of the supplementary 
#' and active elements in the RV space.
#' *** HA comment: Maybe we also want to send the square cosines 
#' to give the quality of representation.
#' @author  Vincent Guillemot
#' @examples
#' \dontrun{
#' # an Example here
#' }
#' @export
supplementalProjection4distatis <- function(res.distatis, elsupp) {
  if (is.na(dim(elsupp)[3])) {
    warning("elsupp is a matrix but should be an array, it has been automatically transformed into a 3D array")
    elsupp <- array(elsupp, dim = c(dim(elsupp), 1))
  }
  
  if (res.distatis$compact) {
    stop("Impossible to project a supplementary element with compact results")
  }
  
  # Double centering
  if (res.distatis$params$double_centering) {
    if (res.distatis$params$Distance) elsupp <- Dist2CP(elsupp)
    else elsupp <- Dist2CP(-elsupp)
  }
  # Normalization
  if (res.distatis$params$Norm == "MFA") {
    elsupp <- CP2MFAnormedCP(elsupp)
  } else if (res.distatis$params$Norm == "SUMPCA") {
    elsupp <- CP2SUMPCAnormedCP(elsupp)
  } else if (res.distatis$params$Norm == "None") {
  } else {
    stop(sprintf("Normalization = %s, but should be either 'MFA', 'SUMPCA' or 'None'.", res.distatis$Norm))
  }
  
  # Compute the projection matrix
  P  <- res.distatis$res4Cmat$eigVector
  G  <- res.distatis$res4Cmat$G
  eV <- res.distatis$res4Cmat$eigValues
  DAT <- res.distatis$res4Splus$SCP
  
  # Compute the RV coefficients between active and supplementary
  rvsupp <- GetRectCmat(DAT, elsupp, RV = res.distatis$params$RV)
  
  # Project the supplementary elements
  Gsupp <- rdiag(t(rvsupp) %*% P , 1 / sqrt(abs(eV)))
  
  if (is.null(dimnames(DAT)[[3]])) {
    idactive <- rep("", dim(DAT)[[3]])
  } else {
    idactive <- dimnames(DAT)[[3]]
  }

  if (is.null(dimnames(elsupp)[[3]])) {
    idsupp <- rep("", dim(elsupp)[[3]])
  } else {
    idsupp <- dimnames(elsupp)[[3]]
  }
  
  dat <- rbind(data.frame(G, ID = idactive),
               data.frame(Gsupp, ID = idsupp))
  dat$Type <- rep(c("Active", "Supplementary"), 
                  c(dim(DAT)[3], dim(elsupp)[3]))
  return(dat)
}