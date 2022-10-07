#' Makes distances and weights for MDS analyses (see \code{\link{epMDS}}).
#' 
#' Makes distances and weights for MDS analyses (see \code{\link{epMDS}}).
#' 
#' 
#' @usage makeDistancesAndWeights(DATA, method = "euclidean", masses = NULL)
#' @param DATA A data matrix to compute distances between row items.
#' @param method which distance metric should be used. \code{method} matches
#' \code{\link{dist}}; Two additional distances are avaialble: "correlation"
#' and "chi2". For "chi2" see \code{\link{chi2Dist}}. Default is "euclidean".
#' @param masses a diagonal matrix (or vector) that contains the masses (for
#' the row items).
#' @return \item{D}{Distance matrix for analysis} \item{MW}{a list item with
#' masses and weights. Weights are not used in \code{\link{epMDS}}.}
#' @author Derek Beaton
#' @seealso \code{link{computeMW}}, \code{link{epMDS}}, \code{link{coreMDS}}
#' @keywords misc multivariate
makeDistancesAndWeights <- function(DATA,method="euclidean",masses=NULL){
	if(method=="chi2"){
		chi2res <- chi2Dist(DATA)
		D <- chi2res$D
		MW <- list(M=chi2res$M)
	}else if(method=="correlation"){
		#why the hell does this not need to be squared?
		D <- (1-cor(t(DATA)))
		MW <- computeMW(D,masses=masses)
	}else{
		D <- as.matrix(dist(DATA,method=method,diag=TRUE,upper=TRUE))
		D <- D^2
		MW <- computeMW(D,masses=masses)		
	}
	return(list(D=D,MW=MW))
}
