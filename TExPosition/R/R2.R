#' R-squared computations
#' 
#' A function to compute R-squared for BADA and DICA
#' 
#' 
#' @param group.masses a masses matrix for the groups
#' @param di a set of squared distances of the groups
#' @param ind.masses a masses matrix for the individuals
#' @param dii a set of squared distances for the individuals
#' @return \item{R2}{An R-squared}
#' @author Jenny Rieck, Derek Beaton
#' @keywords misc multivariate
#' @export R2
R2 <-
function(group.masses,di,ind.masses=NULL,dii){

	
	if(is.null(group.masses)){
		group.masses <- matrix(1/nrow(di),nrow(di),1)
	}
	else if(is.null(dim(group.masses))){
		group.masses <- as.matrix(group.masses)
	}else if(ncol(group.masses)==nrow(group.masses) && nrow(group.masses) == nrow(di)){
		group.masses <- as.matrix(diag(group.masses))
	}else{
		stop('Unknown problem with group.masses in R2 computation.')
	}

	if(is.null(ind.masses)){
		ind.masses <- matrix(1/nrow(dii),nrow(dii),1)
	}
	else if(is.null(dim(ind.masses))){
		ind.masses <- as.matrix(ind.masses)
	}else if(ncol(ind.masses)==nrow(ind.masses) && nrow(ind.masses) == nrow(dii)){
		ind.masses <- as.matrix(diag(ind.masses))
	}else{
		stop('Unknown problem with ind.masses in R2 computation.')		
	}
		
	inertia.between <- sum(group.masses * di)
	inertia.within <- sum(ind.masses * dii)
	
	return(inertia.between/inertia.within)
}
