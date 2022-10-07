#' createDefaultDesign
#' 
#' Creates a default design matrix, wherein all observations (i.e., row items)
#' are in the same group.
#' 
#' 
#' @usage createDefaultDesign(DATA)
#' @param DATA original data that requires a design matrix
#' @return \item{DESIGN}{a column-vector matrix to indicate that all
#' observations are in the same group.}
#' @author Derek Beaton
#' @keywords misc
#' @export createDefaultDesign
createDefaultDesign <-
function(DATA){
		DESIGN <- matrix(1,dim(DATA)[1],1)
		rownames(DESIGN) <- rownames(DATA)	
		return(DESIGN)
}
