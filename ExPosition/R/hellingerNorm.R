#' Hellinger version of CA preprocessing
#' 
#' Performs all steps required for Hellinger form of CA processing (row profile
#' approach).
#' 
#' 
#' @usage hellingerNorm(X, X_dimensions, colTotal, rowTotal, grandTotal,
#' weights = NULL, masses = NULL)
#' @param X Data matrix
#' @param X_dimensions The dimensions of X in a vector of length 2 (rows,
#' columns). See \code{\link{dim}}
#' @param colTotal Vector of column sums.
#' @param rowTotal Vector of row sums.
#' @param grandTotal Grand total of X
#' @param weights Optional weights to include for the columns.
#' @param masses Optional masses to include for the rows.
#' @return \item{rowCenter}{The barycenter of X.} \item{masses}{Masses to be
#' used for the GSVD.} \item{weights}{Weights to be used for the GSVD.}
#' \item{rowProfiles}{The row profiles of X.} \item{deviations}{Deviations of
#' row profiles from \code{rowCenter}.}
#' @author Derek Beaton and Hervé Abdi
#' @export hellingerNorm
hellingerNorm <-
function(X,X_dimensions,colTotal,rowTotal,grandTotal,weights=NULL,masses=NULL){
	
	if(is.null(masses)){
		masses = rowTotal/grandTotal
	}
	if(is.null(weights)){
		weights <- c(matrix(1/ncol(X),1,ncol(X)))
	}
	rowProfiles <- rowNorms(X,type='hellinger')	
	rowCenter <- c(t(as.matrix(masses)) %*% rowProfiles) ##can I make this faster?
	deviations <- rowProfiles - matrix(rowCenter,X_dimensions[1],X_dimensions[2],byrow=TRUE)
	#return(list(rowCenter=rowCenter,masses=masses,M=M,weights=weights,W=W,rowProfiles=rowProfiles,deviations=deviations))
	return(list(rowCenter=rowCenter,masses=masses,weights=weights,rowProfiles=rowProfiles,deviations=deviations))	
}
