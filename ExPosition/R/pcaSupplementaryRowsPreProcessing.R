#' Preprocessing for supplemental rows in PCA.
#' 
#' Preprocessing for supplemental rows in PCA.
#' 
#' 
#' @usage pcaSupplementaryRowsPreProcessing(SUP.DATA = NULL, center = TRUE,
#' scale = TRUE, W = NULL)
#' @param SUP.DATA A supplemental matrix that has the same number of columns as
#' an active data set.
#' @param center The center from the active data. NULL will center
#' \code{SUP.DATA} to itself.
#' @param scale The scale factor from the active data. NULL will scale
#' (z-score) \code{SUP.DATA} to itself.
#' @param W Weights from the active data.
#' @return a matrix that has been preprocessed to project supplementary rows
#' for PCA methods.
#' @author Derek Beaton
#' @keywords misc multivariate
#' @export pcaSupplementaryRowsPreProcessing
pcaSupplementaryRowsPreProcessing <- function(SUP.DATA=NULL,center=TRUE,scale=TRUE,W=NULL){
	if(is.null(SUP.DATA)){
		stop('Must provide supplemental data')
	}
	if(is.null(W)){
		W <- rep(1,ncol(SUP.DATA)) #you need to choose or else I make nothing happen...
	}else if(length(W)!=ncol(SUP.DATA)){
		stop('Length of W does not match column dim of SUP.DATA')
	}
	
	return( expo.scale(SUP.DATA,center=center,scale=scale) * matrix(W,nrow(SUP.DATA),length(W),byrow=TRUE) )
}
