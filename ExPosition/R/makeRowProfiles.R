#' Preprocessing for CA-based analyses
#' 
#' This function performs all preprocessing steps required for Correspondence
#' Analysis-based preprocessing.
#' 
#' 
#' @usage makeRowProfiles(X, weights = NULL, masses = NULL, hellinger = FALSE)
#' @param X Data matrix.
#' @param weights optional. Weights to include in preprocessing.
#' @param masses optional. Masses to include in preprocessing.
#' @param hellinger a boolean. If TRUE, Hellinger preprocessing is used. Else,
#' CA row profile is computed.
#' @return Returns from \code{link{hellingerNorm}} or \code{\link{caNorm}}.
#' @author Derek Beaton
#' @export makeRowProfiles
makeRowProfiles <-
function(X,weights=NULL,masses=NULL,hellinger=FALSE){
	
	X_dimensions <- dim(X)
	colTotal <- colSums(X)
	rowTotal <- rowSums(X)
	grandTotal <- sum(X)
	
	if(hellinger){
		return(hellingerNorm(X,X_dimensions,colTotal,rowTotal,grandTotal,weights,masses))
	}else{
		return(caNorm(X,X_dimensions,colTotal,rowTotal,grandTotal,weights,masses))
	}

}
