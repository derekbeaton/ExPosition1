corePCA <-
function(DATA,M=NULL,W=NULL,decomp.approach='svd',k=0){

	DATA_dims <- dim(DATA)
#DATA comes in already scaled & centered or not. That happens at the PCA	& BADA level.
	
	if(is.null(M)){
		M <- rep(1,nrow(DATA))
	}
	if(is.null(W)){
		W <- rep(1,ncol(DATA))
	}	
	
	#vectorize internally to this function. ##VERIFY THIS AND MAKE IT AVAIALABLE EVERYWHERE.
	if((!is.null(dim(M))) && (length(M) == (nrow(M) * ncol(M)))){
		M <- diag(M)
	}
	if((!is.null(dim(W))) && (length(W) == (nrow(W) * ncol(W)))){
		W <- diag(W)
	}		
	
	pdq_results <- genPDQ(datain=DATA,M=M,W=W,is.mds=FALSE,decomp.approach=decomp.approach,k=k)

	####TURN BELOW INTO A FUNCTION.
	fi <- matrix(M,nrow(pdq_results$p),ncol(pdq_results$p)) * pdq_results$p * 
			matrix(pdq_results$Dv,nrow(pdq_results$p),ncol(pdq_results$p),byrow=TRUE)
	rownames(fi) <- rownames(DATA)		
	di <- rowSums(fi^2)
	ri <- matrix(1/di,nrow(fi),ncol(fi)) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)
	ci <- matrix(1/M,nrow(fi),ncol(fi),byrow=FALSE) * (fi^2)/
		matrix(pdq_results$Dv^2,nrow(fi),ncol(fi),byrow=TRUE)
	ci <- replace(ci,is.nan(ci),0)	
	di <- as.matrix(di)		

	#columns
	fj <- matrix(W,nrow(pdq_results$q),ncol(pdq_results$q)) * pdq_results$q * 
			matrix(pdq_results$Dv,nrow(pdq_results$q),ncol(pdq_results$q),byrow=TRUE)
	rownames(fj) <- colnames(DATA)		
	dj <- rowSums(fj^2)
	rj <- matrix(1/dj,nrow(fj),ncol(fj)) * (fj^2)
	rj <- replace(rj,is.nan(rj),0)
	cj <- matrix(1/W,nrow(pdq_results$q),ncol(pdq_results$q),byrow=FALSE) * (fj^2)/
		matrix(pdq_results$Dv^2,nrow(fj),ncol(fj),byrow=TRUE)
	cj <- replace(cj,is.nan(cj),0)	
	dj <- as.matrix(dj)	

	#I can append the masses & weights if necessary in the appropriate functions
	res <- list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=pdq_results$tau,eigs=pdq_results$eigs,pdq=pdq_results,X=DATA,M=M,W=W)
}
