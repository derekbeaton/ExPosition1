#' texpoDesignCheck
#' 
#' TExPosition's DESIGN matrix check function. Calls into ExPosition's
#' \code{\link[ExPosition]{designCheck}}.
#' 
#' For BADA & DICA, execution stops if:\cr 1. DESIGN has more columns (groups)
#' than observations, 2. DESIGN has only 1 column (group), or 3. DESIGN has at
#' least 1 occurence where an observation is the only observation in a group
#' (i.e., colSums(DESIGN)==1 at least once).
#' 
#' @param DATA original data that should be matched to a design matrix
#' @param DESIGN a column vector with levels for observations or a dummy-coded
#' matrix
#' @param make_design_nominal a boolean. Will make DESIGN nominal if TRUE
#' (default).
#' @param force_bary a boolean. If TRUE, it forces the check for barycentric
#' methods (tepDICA, tepBADA). If FALSE, \code{\link[ExPosition]{designCheck}} is
#' performed.
#' @return \item{DESIGN}{dummy-coded design matrix}
#' @author Derek Beaton
#' @keywords misc
#' @export texpoDesignCheck
texpoDesignCheck <-
function(DATA=NULL,DESIGN=NULL,make_design_nominal=TRUE,force_bary = FALSE){
	
	
	if(!is.null(DATA)){ #this is a short cut and failsafe for all TEx Methods. PLS can go through, but barycentric methods must also pass the tests below.
		DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	}
	if(force_bary){	
		if(ncol(DESIGN) >= nrow(DESIGN)){
			stop('You have too many groups, try a method in ExPosition.')
		}
		if(ncol(DESIGN)==1){
			stop('You have too few groups, try a method in ExPosition.')		
		}
		if(!sum(colSums(DESIGN)>1)==ncol(DESIGN)){
			#this might be too stringent...
			stop('You have at least 1 group made up of only 1 observation, try a method in ExPosition.')
		}
	}
	return(DESIGN)
}
