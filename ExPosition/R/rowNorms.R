#' Normalize the rows of a matrix.
#' 
#' This function will normalize the rows of a matrix.
#' 
#' rowNorms works like \code{link{expo.scale}}, but for rows. Hellinger row
#' norm via \code{hellinger}, Correspondence analysis analysis row norm (row
#' profiles) via \code{ca}, Z-score row norm via \code{z}. \code{other} passes
#' \code{center} and \code{scale} to \code{\link{expo.scale}} and allows for
#' optional centering and scaling parameters.
#' 
#' @usage rowNorms(X, type = NULL, center = FALSE, scale = FALSE)
#' @param X Data matrix
#' @param type a string. Type of normalization to perform. Options are
#' \code{hellinger}, \code{ca}, \code{z}, \code{other}
#' @param center optional. A vector to center the columns of X.
#' @param scale optional. A vector to scale the values of X.
#' @return Returns a row normalized version of X.
#' @author Derek Beaton
#' @export rowNorms
rowNorms <- function(X,type=NULL,center=FALSE,scale=FALSE){
	
	if(is.null(type)){
		return(X)
	}else if(type=='hellinger'){
		return(sqrt(rowNorms(X,type="ca")))
	}else if(type == 'ca'){
		return(X/matrix(rowSums(X),nrow(X),ncol(X)))
	}else if (type == 'z'){
		return(t(expo.scale(t(X),center=TRUE,scale=TRUE)))
	}else if(type == 'other'){
		return(t(expo.scale(t(X),center=center,scale=scale)))
	}else{
		return(X)
	}
	
}
