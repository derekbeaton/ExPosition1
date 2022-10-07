#' Perform Rv coefficient computation.
#' 
#' Perform Rv coefficient computation.
#' 
#' 
#' @usage rvCoeff(Smat, Tmat, type)
#' @param Smat A square covariance matrix
#' @param Tmat A square covariance matrix
#' @param type DEPRECATED. Any value here will be ignored
#' @return A single value that is the Rv coefficient.
#' @author Derek Beaton
#' @references Robert, P., & Escoufier, Y. (1976). A Unifying Tool for Linear
#' Multivariate Statistical Methods: The RV-Coefficient. \emph{Journal of the
#' Royal Statistical Society. Series C (Applied Statistics)}, \emph{25}(3),
#' 257--265.
#' @keywords misc multivariate
#' @export rvCoeff
rvCoeff <- function(Smat,Tmat, type){
##private function
matrixTrace <- function(squareMatrix){
	return(sum(diag(squareMatrix)))
}	
	#rv = 0
	if(sum(dim(Smat) == dim(Tmat)) != 2){
		print('Dimensions do not match')
		return(NaN)
	}
		
	return( as.numeric(matrixTrace(t(Smat) %*% Tmat) / sqrt( matrixTrace(t(Smat) %*% Smat) %*% matrixTrace(t(Tmat) %*% Tmat) )) ) 
	
}
