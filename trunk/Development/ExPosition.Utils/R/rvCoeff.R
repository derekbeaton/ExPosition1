rvCoeff <- function(Smat,Tmat){
##private function
matrixTrace <- function(squareMatrix){
	return(sum(diag(squareMatrix)))
}	
	
	#rv = 0
	if(sum(dim(Smat) == dim(Tmat)) != 2){
		print('Dimensions do not match')
		return(NaN)
	}
		
	return( as.numeric(matrixTrace(t(Smat) %*% Tmat) / sqrt( matrixTrace(t(Smat) %*% Smat) %*% matrixTrace(t(Tmat) %*% Tmat) )) ) 
	
}
