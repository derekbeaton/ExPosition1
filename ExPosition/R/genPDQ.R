#' genPDQ: the GSVD
#' 
#' genPDQ performs the SVD and GSVD for all methods in
#' \code{\link{ExPosition}}.
#' 
#' This function should only be used to create new methods based on the SVD or
#' GSVD.
#' 
#' @usage genPDQ(datain, M = NULL, W = NULL, is.mds = FALSE, decomp.approach =
#' "svd", k = 0)
#' @param datain fully preprocessed data to be decomposed.
#' @param M vector of masses (for the rows)
#' @param W vector of weights (for the columns)
#' @param is.mds a boolean. If the method is of MDS (e.g.,
#' \code{\link{epMDS}}), use TRUE. All other methods: FALSE
#' @param decomp.approach a string. Allows for the user to choose which
#' decomposition method to perform. Current options are SVD or Eigen.
#' @param k number of components to return (this is not a rotation, just an
#' \emph{a priori} selection of how much data should be returned).
#' @return Data of class \code{epSVD} which is a list of matrices and
#' vectors:\cr \item{P}{The left singular vectors (rows).} \item{Q}{The right
#' singular vectors (columns).} \item{Dv}{Vector of the singular values.}
#' \item{Dd}{Diagonal matrix of the singular values.} \item{ng}{Number of
#' singular values/vectors} \item{rank}{Rank of the decomposed matrix. If it is
#' 1, 0s are padded to the above items for plotting purposes.}
#' \item{tau}{Explained variance per component}
#' @author Derek Beaton
#' @seealso \code{\link{pickSVD}}
#' @export genPDQ
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
		M <- sqrt_mat(M)
		W <- sqrt_mat(W)
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
