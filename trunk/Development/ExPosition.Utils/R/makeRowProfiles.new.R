makeRowProfiles.new <-
function(X,weights=NULL,masses=NULL,hellinger=FALSE,spherical=FALSE){
	masses <- rowSums(X)/sum(X)
	rowCenter <- colSums(X)/sum(X)
	rowProfiles <- rowNorms(X,type="ca")
	
	if(hellinger){
		weights <- 1/ncol(X)
		alpha <- 0.5
	}else{
		weights <- 1/rowCenter
		alpha <- 1
	}
	
	if(spherical){ ###his should be the given masses, never the computed.
		the.center <- matrix(masses %*% sqrt(rowProfiles),nrow(N),ncol(N),byrow=TRUE)
	}else{
		the.center <- matrix(rowCenter,nrow(N),ncol(N),byrow=TRUE)^alpha
	}	
	deviations <- (rowProfiles^alpha - the.center)
	
	return(list(rowCenter=rowCenter,masses=masses,weights=weights,rowProfiles=rowProfiles,deviations=deviations))	
}