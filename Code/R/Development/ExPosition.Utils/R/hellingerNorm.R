hellingerNorm <-
function(X,X_dimensions,colTotal,rowTotal,grandTotal,weights=NULL,masses=NULL){
	
	if(is.null(masses)){
		masses = rowTotal/grandTotal
	}
	if(is.null(weights)){
		weights <- c(matrix(1/ncol(X),1,ncol(X)))
	}
	rowProfiles <- rowNorms(X,type='hellinger')	
	rowCenter <- c(t(as.matrix(masses)) %*% rowProfiles) ##can I make this faster?
	deviations <- rowProfiles - matrix(rowCenter,X_dimensions[1],X_dimensions[2],byrow=TRUE)
	#return(list(rowCenter=rowCenter,masses=masses,M=M,weights=weights,W=W,rowProfiles=rowProfiles,deviations=deviations))
	return(list(rowCenter=rowCenter,masses=masses,weights=weights,rowProfiles=rowProfiles,deviations=deviations))	
}
