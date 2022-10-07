#' Preprocessing for supplementary columns in PCA.
#' 
#' Preprocessing for supplementary columns in PCA.
#' 
#' 
#' @usage pcaSupplementaryColsPreProcessing(SUP.DATA = NULL, center = TRUE,
#' scale = TRUE, M = NULL)
#' @param SUP.DATA A supplemental matrix that has the same number of rows as an
#' active data set.
#' @param center The center from the active data. NULL will center
#' \code{SUP.DATA} to itself.
#' @param scale The scale factor from the active data. NULL will scale
#' (z-score) \code{SUP.DATA} to itself.
#' @param M Masses from the active data.
#' @return a matrix that has been preprocessed to project supplementary columns
#' for PCA methods.
#' @author Derek Beaton
#' @keywords misc multivariate
#' @export pcaSupplementaryColsPreProcessing
pcaSupplementaryColsPreProcessing <- function(SUP.DATA=NULL,center=TRUE,scale=TRUE,M=NULL){
	
	if(is.null(SUP.DATA)){
		stop('Must provide supplemental data')
	}
	if(is.null(M)){
		M <- rep(1,nrow(SUP.DATA)) #you need to choose or else I make nothing happen...
	}else if(length(M)!=nrow(SUP.DATA)){
		stop('Length of M does not match row dim of SUP.DATA')
	}
  
	return( t( expo.scale(SUP.DATA,center=center,scale=scale) * matrix(M,length(M),ncol(SUP.DATA)) ) )
}
