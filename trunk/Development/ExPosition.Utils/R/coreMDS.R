coreMDS <-
function(DATA,masses=NULL,decomp.approach='svd',k=0){

	DATA_dims <- dim(DATA)
	#DATA comes in AS A DISTANCE MATRIX. That happens at the MDS or MDS-extension level.

	if(is.null(masses)){
		masses <- rep(1/DATA_dims[1],DATA_dims[1])
	}
	if((!is.null(dim(masses))) && (length(masses) == (nrow(masses) * ncol(masses)))){
		masses <- diag(masses)
	}
		
	S <- mdsTransform(DATA,masses)
	#pdq_results <- basePDQ(S,is.mds=TRUE,decomp.approach=decomp.approach,k=k)
	pdq_results <- genPDQ(datain=S,is.mds=TRUE,decomp.approach=decomp.approach,k=k)
	

	fi <- matrix(1/sqrt(masses),nrow=length(masses),ncol=length(pdq_results$Dv)) * (pdq_results$p * matrix(sqrt(pdq_results$Dv),nrow(pdq_results$p),ncol(pdq_results$p),byrow=TRUE))		
	rownames(fi) <- rownames(DATA)		
	di <- rowSums(fi^2)
	ri <- matrix(1/di,nrow(fi),ncol(fi)) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)	
	ci <- matrix(masses,nrow(fi),ncol(fi),byrow=FALSE) * (fi^2)/
		matrix(pdq_results$Dv^2,nrow(fi),ncol(fi),byrow=TRUE)
	ci <- replace(ci,is.nan(ci),0)	
	di <- as.matrix(di)		

	#I can append the masses & weights if necessary in the appropriate functions
	res <- list(fi=fi,di=di,ci=ci,ri=ri,masses=masses,t=pdq_results$tau,eigs=pdq_results$Dv,pdq=pdq_results,X=S)
}
