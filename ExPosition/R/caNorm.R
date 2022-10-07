#' Correspondence analysis preprocessing
#' 
#' Performs all steps required for CA processing (row profile approach).
#' 
#' 
#' @usage caNorm(X, X_dimensions, colTotal, rowTotal, grandTotal, weights =
#' NULL, masses = NULL)
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
#' @author Derek Beaton
#' @export caNorm
caNorm <-
function(X,X_dimensions,colTotal,rowTotal,grandTotal,weights=NULL,masses=NULL){

	rowCenter = colTotal/grandTotal
	if(is.null(masses)){
		masses = rowTotal/grandTotal
	}
	if(is.null(weights)){
		weights = rowCenter^-1
	}
	rowProfiles <- rowNorms(X,type='ca')
	deviations <- rowProfiles - matrix(rowCenter,X_dimensions[1],X_dimensions[2],byrow=TRUE)
	
	return(list(rowCenter=rowCenter,masses=masses,weights=weights,rowProfiles=rowProfiles,deviations=deviations))
}
