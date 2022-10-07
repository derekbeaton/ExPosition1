#' Pick which generalized SVD (or related) decomposition to use.
#' 
#' This function is an interface for the user to a general SVD or related
#' decomposition. It provides direct access to \code{\link{svd}} and
#' \code{\link{eigen}}. Future decompositions will be available.
#' 
#' 
#' @usage pickSVD(datain, is.mds = FALSE, decomp.approach = "svd", k = 0)
#' @param datain a data matrix to decompose.
#' @param is.mds a boolean. TRUE for a MDS decomposition.
#' @param decomp.approach a string. 'svd' for singular value decomposition,
#' 'eigen' for an eigendecomposition. All approaches provide identical output.
#' Some approaches are (in some cases) faster than others.
#' @param k numeric. The number of components to return.
#' @return A list with the following items:\cr \item{u}{Left singular vectors
#' (rows)} \item{v}{Right singular vectors (columns)} \item{d}{Singular values}
#' \item{tau}{Explained variance per component}
#' @author Derek Beaton
#' @keywords misc multivariate
#' @export pickSVD
pickSVD <-
function(datain,is.mds=FALSE,decomp.approach='svd',k=0){

	dataDims <- dim(datain)
	I <- dataDims[1]
	J <- dataDims[2]
	m <- min(I,J)


	#check k
	k.is.percent <- FALSE
	if(k > 0 && k < 1){
		k.is.percent <- TRUE
	}
	if(!k.is.percent){
		k <- ceiling(k)	
	}
	if(k <= 0 || k > m){
		k <- m
	}

	
	flip <- FALSE	
	if (I < J){
		datain <- t(datain)
		flip <- TRUE
	}	
	
	#check decomp.approach
	if(is.null(decomp.approach)){
		decomp.approach <- 'svd'
	}
	#if there are more than 1 million elements or you want to go fast, do the eigen decomp.approach!
	num.el <- (I * J)
	if(num.el > 1000000){
		decomp.approach <- 'eigen'
	}

	##for now, only eigen & svd.
	if(tolower(decomp.approach)=='eigen'){
		eigOut <- eigen(t(datain) %*% datain)	
		Q <- eigOut$vectors
		d <- sqrt(eigOut$values)
		P <- datain %*% Q %*% diag(d^-1)

	}else{ ##the default method.
		if(k.is.percent){
			svd.out <- svd(datain)
		}else{ ##this just helps a bit with speed and memory and accurate tau.
			svd.out <- svd(datain,nu=k,nv=k)
		}
		P <- svd.out$u
		Q <- svd.out$v
		d <- svd.out$d		
	}
	##but we hope to add faster methods soon, e.g., RcppArmadillo
	if(flip){
		temp<-Q
		Q<-P
		P<-temp
		rownames(Q) <- rownames(datain)
		rownames(P) <- colnames(datain)		
	}else{
		rownames(P) <- rownames(datain)
		rownames(Q) <- colnames(datain)			
	}
	
	
	#find precision limit, get rid of junk comps.
	precisionLimit <- .Machine$double.eps	
	if(is.mds){
		eigs <- d
	}else{
		eigs <- d^2		
	}
	indToKeep <- which(eigs > precisionLimit)	
	eigs <- eigs[indToKeep]	
	tau <- eigs/sum(eigs)	##value could be small due to error.

	viz.comps <- intersect(intersect(indToKeep,1:ncol(P)),intersect(indToKeep,1:ncol(Q)))

	P <- as.matrix(P[,viz.comps])
	Q <- as.matrix(Q[,viz.comps])
	d <- d[viz.comps]
	
	if(k.is.percent){
		k <- 1:min(which(cumsum(tau) > k))
		P <- as.matrix(P[,k])
		Q <- as.matrix(Q[,k])
		d <- d[k]	
	}
	
	no.dims <- (is.null(dim(P)) && is.null(dim(Q)))
	dim.1 <- (ncol(P)==1 && ncol(Q)==1)
	if( no.dims || dim.1 ){
		P <- cbind(P,0)
		Q <- cbind(Q,0)
		d <- c(d,0)
		warning('Solution has only 1 singular vector (or value). Zeros are appended for plotting purposes.')
	}
	
	return(list(u=P,v=Q,d=d,tau=tau*100,eigs=eigs))
}
