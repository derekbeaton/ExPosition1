coreCA <-
function(DATA,masses=NULL,weights=NULL,hellinger=FALSE,symmetric=TRUE,decomp.approach='svd',k=0){

	DATA_dimensions = dim(DATA)
	
	
	###PERHAPS ALL OF THIS SHOULD OCCUR IN THE CA FUNCTION?
	mRP<-makeRowProfiles(DATA,weights=weights,masses=masses,hellinger=hellinger)

	pdq_results <- genPDQ(datain=mRP$deviations,M=mRP$masses,W=mRP$weights,is.mds=FALSE,decomp.approach=decomp.approach,k=k)	

	
	#Rows, F
	fi <- pdq_results$p * matrix(pdq_results$Dv,nrow(pdq_results$p),ncol(pdq_results$p),byrow=TRUE)
	rownames(fi) <- rownames(DATA)	
	di <- rowSums(fi^2)
	ri <- matrix(1/di,nrow(fi),ncol(fi)) * (fi^2)
	ri <- replace(ri,is.nan(ri),0)	
	ci <- matrix(mRP$masses,nrow(fi),ncol(fi)) * (fi^2)/
		matrix(pdq_results$Dv^2,nrow(fi),ncol(fi),byrow=TRUE)	
	ci <- replace(ci,is.nan(ci),0)
	di <- as.matrix(di)

	###this could be cleaned up. But, after I overhaul CA on the whole.
	fj <- matrix(mRP$weights,nrow(pdq_results$q),ncol(pdq_results$q)) * pdq_results$q * 
			matrix(pdq_results$Dv,nrow(pdq_results$q),ncol(pdq_results$q),byrow=TRUE)
	rownames(fj) <- colnames(DATA)		
	cj <- matrix(1/mRP$weights,nrow(fj),ncol(fj)) * (fj^2) /
		matrix(pdq_results$Dv^2,nrow(fj),ncol(fj),byrow=TRUE)	
	cj <- replace(cj,is.nan(cj),0)		
	if(!symmetric){
		fj <- fj * matrix(pdq_results$Dv^-1,nrow(pdq_results$q),ncol(pdq_results$q),byrow=TRUE)
	}
	dj <- rowSums(fj^2)
	rj <- matrix(1/dj,nrow(fj),ncol(fj)) * (fj^2)
	rj <- replace(rj,is.nan(rj),0)
	dj <- as.matrix(dj)			
	
	return(list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=pdq_results$tau,eigs=pdq_results$Dv^2,M=mRP$masses,W=mRP$weights,c= mRP$rowCenter,pdq=pdq_results,X=mRP$deviations,hellinger=hellinger,symmetric=symmetric))
}
