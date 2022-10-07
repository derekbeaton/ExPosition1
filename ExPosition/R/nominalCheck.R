#' Checks if data are disjunctive.
#' 
#' Checks if data is in disjunctive (sometimes called complete binary) format.
#' To be used with MCA (e.g., \code{\link{epMCA}}).
#' 
#' 
#' @usage nominalCheck(DATA)
#' @param DATA A data matrix to check. This should be 0/1 disjunctive coded.
#' \code{nominalCheck} just checks to make sure it is complete.
#' @return If \code{DATA} are nominal, \code{DATA} is returned. If not,
#' \code{\link{stop}} is called and execution halts.
#' @author Derek Beaton
#' @keywords misc multivariate multiple correspondence analysis
#' @export nominalCheck
nominalCheck <- function(DATA){
	data.dim <- dim(DATA)
	colSums.data <- colSums(DATA)
	orig.cols <- sum(cumsum(colSums.data) %% data.dim[1]==0)	
	
	if( (!max(cumsum(colSums.data) / data.dim[1]) == orig.cols) || (!sum(cumsum(colSums.data) %% data.dim[1] == 0) == orig.cols)){
		#ESCAPE!
		stop("Data is not nominal (disjunctive).")
	}else{
		return(DATA)
	}
	
}
