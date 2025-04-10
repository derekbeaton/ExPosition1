##I almost certainly don't need to send the whole set of data along, do I? This is probably a waste. FIX THIS IN FUTURE RELEASE.


#' Compute indicies for bootstrap resampling.
#' 
#' This function computes a set of indicies for bootstrap resampling. It can be
#' unconstrained or bootstrap within a group design.
#' 
#' 
#' @param DATA The original data matrix to be bootstrapped. Rows will be
#' bootstrapped and are assumed to be observations.
#' @param DESIGN A design matrix (in disjunctive coding). Only used if
#' \code{constrained} is TRUE.
#' @param constrained a boolean. If TRUE, bootstrap resampling will occur
#' within groups as designated by the \code{DESIGN} matrix.
#' @return a set of indicies to be used to be used as the bootstrap resampled
#' indices.
#' @author Derek Beaton
#' @seealso \code{\link{boot.compute.fj}} and \code{\link{boot.ratio.test}}
#' @export boot.samples
#' @keywords bootstrap
#' @examples
#' 
#' 	data(ep.iris)
#' 	unconstrained.indices <- boot.samples(ep.iris$data)
#' 	#ep.iris$data[unconstrained.indices,]
#' 	constrained.indices <- boot.samples(ep.iris$data,DESIGN=ep.iris$design,constrained=TRUE)
#' 	#ep.iris$data[constrained.indices,]	
#' 
boot.samples <- function(DATA,DESIGN=NULL,constrained=FALSE){
	if(constrained && !is.null(DESIGN)){
		boot.index <- vector()
		for(i in 1:ncol(DESIGN)){
			boot.index <- c(boot.index,sample(which(DESIGN[,i]==1),replace=TRUE))
		}
	}else{
		boot.index <- sample(nrow(DATA),nrow(DATA),TRUE)
	}
	
	return(boot.index)
}
