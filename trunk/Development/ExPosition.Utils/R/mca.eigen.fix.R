mca.eigen.fix <-
function(DATA,mca.results,make_data_nominal=TRUE,numVariables=NULL,correction=c('b'),symmetric=FALSE){


#can I make this more efficient?
#with large data, especially with MCA, this stuff will get very large very fast.			
	if('b' %in% correction){
		#print('Benzecri correction selected')		
		orig.pdq_results <- mca.results$pdq
		new.pdq_results <- mca.results$pdq	

		masses <- mca.results$M
		#masses <- diag(M)
		weights <- mca.results$W
		#weights <- diag(W)
		if(make_data_nominal){
			DATA <- makeNominalData(DATA)
		}
		rowCenter <- colSums(DATA)/sum(DATA)
	
		if(is.null(numVariables)){
				numVariables <- sum(DATA[1,]>=1)
		}
		eigvals <- new.pdq_results$Dv^2
		new_eigs <- benzecri.eigenfix(eigvals,numVariables)
		#new.pdq_results$Dv <- new_eigs^(1/2)
		new.pdq_results$Dv <- sqrt(new_eigs)
		new.pdq_results$Dd <- diag(new.pdq_results$Dv)
		new.pdq_results$ng <- length(new.pdq_results$Dv)
		new.pdq_results$p <- new.pdq_results$p[,1:new.pdq_results$ng]
		new.pdq_results$q <- new.pdq_results$q[,1:new.pdq_results$ng]
			
		if('g' %in% correction){
			#print('Greenacre adjustment selected.')
			taus <- greenacre.tau.adjust.benzecri(orig.pdq_results$Dv^2,numVariables,new.pdq_results$Dv^2,nrow(mca.results$fj))
		}else{
			taus <- (new.pdq_results$Dv^2/sum(new.pdq_results$Dv^2))*100
		}
		
		if(sum(dim(new.pdq_results$Dd)==0)){
			print('Corrections have failed. Original information must be used.')
			new.pdq_results <- orig.pdq_results
		}
		
		
		###ALL OF THIS SHOULD BE CHANGED. 		
		fi <- new.pdq_results$p * matrix(new.pdq_results$Dv,nrow(new.pdq_results$p),ncol(new.pdq_results$p),byrow=TRUE)
		rownames(fi) <- rownames(mca.results$fi)
		di <- rowSums(fi^2)
		ri <- matrix(1/di,nrow(fi),ncol(fi)) * (fi^2)
		ri <- replace(ri,is.nan(ri),0)	
		ci <- matrix(masses,nrow(fi),ncol(fi)) * (fi^2)/
			matrix(new.pdq_results$Dv^2,nrow(fi),ncol(fi),byrow=TRUE)	
		ci <- replace(ci,is.nan(ci),0)
		di <- as.matrix(di)


		fj <- repmat(weights,1, new.pdq_results$ng) * new.pdq_results$q * 
			matrix(new.pdq_results$Dv,nrow(new.pdq_results$q),ncol(new.pdq_results$q),byrow=TRUE)
		rownames(fj) <- rownames(mca.results$fj)
		cj <- matrix(1/weights,nrow(fj),ncol(fj)) * (fj^2) /
			matrix(new.pdq_results$Dv^2,nrow(fj),ncol(fj),byrow=TRUE)	
		cj <- replace(cj,is.nan(cj),0)
		if(!symmetric){
			fj <- fj * matrix(new.pdq_results$Dv^-1,nrow(new.pdq_results$q),ncol(new.pdq_results$q),byrow=TRUE)
		}
		dj <- rowSums(fj^2)
		rj <- matrix(1/dj,nrow(fj),ncol(fj)) * (fj^2)
		rj <- replace(rj,is.nan(rj),0)
		dj <- as.matrix(dj)			

		
		res <- list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=taus,eigs=new.pdq_results$Dv^2,M=masses,W=weights,pdq=new.pdq_results,pdq.uncor= orig.pdq_results,X=mca.results$X,hellinger=mca.results$hellinger,symmetric=mca.results$symmetric)
		class(res) <- c("epMCA","list")
		return(res)
	}
	#else if('g' %in% correction){
	#	print('Only Greenacre adjustment selected.')
	#	taus <- greenacre.tau.adjust.benzecri(orig.pdq_results$Dv^2,numVariables,nrow(mca.results$fj))
	#}
	else{
		#print('No Correction Selected. Results unchanged.')
		return(mca.results)
	}
	
}
