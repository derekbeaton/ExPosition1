genPDQ <- function(datain,M=NULL,W=NULL,is.mds=FALSE,decomp.approach='svd',k=0){
		
	na.check <- is.na(datain)
	nan.check <- is.nan(datain)
	inf.check <- is.infinite(datain)
	if(sum( na.check | nan.check | inf.check )>0){
		stop("ExPosition must stop. There are NA, NaN, or Infinte values.")
	}

	if(is.null(M)){
		M <- rep(1,nrow(datain))
	}
	if(is.null(W)){
		W <- rep(1,ncol(datain))
	}


	if((is.null(dim(M)) && is.null(dim(W))) && (length(W)>0 && length(M)>0)){
		vectorflag <- TRUE
		M <- sqrt(M)
		W <- sqrt(W)
	}else if(length(dim(M))==2 && length(dim(W))==2){
		vectorflag <- FALSE
		M <- sqrt.mat(M)
		W <- sqrt.mat(W)
	}else{
		stop("There is an error in the formatting of your masses or weights")
	}
	
	if(vectorflag){
		#datain <- matrix(sqrt(M),length(M),dim(datain)[2],byrow=FALSE) * datain * matrix(sqrt(W),dim(datain)[1],length(W),byrow=TRUE)
		datain <- matrix(M,length(M),dim(datain)[2],byrow=FALSE) * datain * matrix(W,dim(datain)[1],length(W),byrow=TRUE)
	}else{
		#datain <- (M^(1/2)) %*% datain %*% (W^(1/2))
		datain <- M %*% datain %*% W
	}

	#shipping off the call!	
	svdOUT <- pickSVD(datain,is.mds=is.mds,decomp.approach=decomp.approach,k=k)
	
	#now get data into PDQ
	P <- svdOUT$u
	d <- as.vector(svdOUT$d)	
	D <- diag(d)
	Q <- svdOUT$v
	tau <- svdOUT$tau
	eigs <- svdOUT$eigs
	rank <- length(eigs)
	

	if(vectorflag){
		P <- matrix(1/M,dim(P)[1],dim(P)[2],byrow=FALSE) * P
		Q <- matrix(1/W,dim(Q)[1],dim(Q)[2],byrow=FALSE) * Q
	}else{
		#P <- matrix(1/sqrt(diag(M)),dim(P)[1],dim(P)[2],byrow=FALSE) * P
		#Q <- matrix(1/sqrt(diag(W)),dim(Q)[1],dim(Q)[2],byrow=FALSE) * Q		
		P <- (1/M) %*% P
		Q <- (1/W) %*% Q		
	}
	
	res <- list(p=P,q=Q,Dv=d,Dd=D,ng=length(d),rank=rank,tau=tau,eigs=eigs)
	class(res) <- c("epSVD","list")	
	return(res)
	
}
