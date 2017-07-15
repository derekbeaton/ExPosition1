##stolen from http://stackoverflow.com/questions/11057639/identifying-if-only-the-diagonal-is-non-zero
isDiagonal.matrix <- function(X){
	if(is.null(dim(X))){
		stop("X is not a matrix.")
	}
	return(all(X[lower.tri(X)] == 0, X[upper.tri(X)] == 0))
	
}