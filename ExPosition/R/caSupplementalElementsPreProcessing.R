#' Correspondence Analysis preprocessing.
#' 
#' CA preprocessing for data. Can be performed on rows or columns of your data.
#' This is a row-profile normalization.
#' 
#' 
#' @usage caSupplementalElementsPreProcessing(SUP.DATA)
#' @param SUP.DATA Data that will be supplemental. Row profile normalization is
#' used. For supplemental rows use \code{t(SUP.DATA)}.
#' @return returns a matrix that is preprocessed for supplemental projections.
#' @author Derek Beaton
#' @seealso \code{\link{mdsSupplementalElementsPreProcessing}},
#' \code{\link{pcaSupplementaryColsPreProcessing}},
#' \code{\link{pcaSupplementaryRowsPreProcessing}},
#' \code{\link{hellingerSupplementaryColsPreProcessing}},
#' \code{\link{hellingerSupplementaryRowsPreProcessing}},
#' \code{\link{supplementaryCols}}, \code{\link{supplementaryRows}},
#' \code{\link{supplementalProjection}}, \code{\link{rowNorms}}
#' @keywords misc multivariate
#' @export caSupplementalElementsPreProcessing
caSupplementalElementsPreProcessing <- function(SUP.DATA){

	return(rowNorms(SUP.DATA,'ca'))
	
}
