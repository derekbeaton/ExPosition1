caNorm <-
function(X,X_dimensions,colTotal,rowTotal,grandTotal,weights=NULL,masses=NULL){

	rowCenter = colTotal/grandTotal
	if(is.null(masses)){
		masses = rowTotal/grandTotal
	}
	if(is.null(weights)){
		weights = rowCenter^-1
	}
	rowProfiles <- rowNorms(X,type='ca')
	deviations <- rowProfiles - matrix(rowCenter,X_dimensions[1],X_dimensions[2],byrow=TRUE)
	
	return(list(rowCenter=rowCenter,masses=masses,weights=weights,rowProfiles=rowProfiles,deviations=deviations))
}
