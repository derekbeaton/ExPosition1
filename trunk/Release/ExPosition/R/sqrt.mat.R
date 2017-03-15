##a function specifically for sqrt'ing a matrix for the GSVD weights.
sqrt.mat <- function(X){
	
	##first, test if it is symmetric in size
	if(!isSymmetric.matrix(X)){
		stop("Weight/Mass matrix is not symmetric")
	}
	##test if it is a diagonal matrix -- then just sqrt it and send back.
	if(isDiagonal.matrix(X)){
		return(  sqrt(diag(X)) ) ##return the vector so we can be fast about it.
	}else{
		A <- eigen(X)
		##change values below tolerance
		A$values[which(A$values < .Machine$double.eps)] <- 0
		##first, test if positive definite
		if( sum(A$values < 0 )>0 ){
			stop("Weight/Mass matrix not positive definite. Some eigenvalues are less than 0")	
		}else{		
			return(A$vectors %*% diag(sqrt(A$values)) %*% t(A$vectors))
		}
	}
}