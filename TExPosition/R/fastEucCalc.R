#' fastEucCalc
#' 
#' Fast Euclidean distance calculations.
#' 
#' This function is especially useful for discriminant analyses. The distance
#' from each point in \code{x} to each point in \code{c} is computed and
#' returned as a \code{nrow(x)} x \code{nrow(c)} matrix.
#' 
#' @param x a set of points.
#' @param c a set of centers.
#' @return \item{a distance matrix}{Euclidean distances of each point to each
#' center are returned.}
#' @author Hervé Abdi, Derek Beaton
#' @keywords misc
#' @export fastEucCalc
fastEucCalc <-
function(x,c){
	
	if(ncol(x) == 1){
		return((x^2) %*% matrix(1,1,nrow(c)) + matrix(1,nrow(x),1) %*% t((c^2))-2 * x %*% t(c))
	}
	else{ 
		x2=colSums(t(x)^2)
		c2=colSums(t(c)^2)
		return(x2 %*% matrix(1,1,nrow(c))+ matrix(1,nrow(x),1) %*% c2-(2 * x %*% t(c)))
	}
}
