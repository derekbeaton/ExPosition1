#' corePCA
#' 
#' corePCA performs the core of principal components analysis (PCA), and
#' related techniques.
#' 
#' This function should not be used directly. Please use \code{\link{epPCA}}
#' unless you plan on writing extensions to ExPosition.
#' 
#' @usage corePCA(DATA, M = NULL, W = NULL, decomp.approach = 'svd', k = 0)
#' @param DATA original data to decompose and analyze via the singular value
#' decomposition.
#' @param M a vector or diagonal matrix with masses for the rows
#' (observations). If NULL, one is created or the plain SVD is used.
#' @param W a vector or diagonal matrix with weights for the columns
#' (measures). If NULL, one is created or the plain SVD is used.
#' @param decomp.approach string. A switch for different decompositions
#' (typically for speed). See \code{\link{pickSVD}}.
#' @param k number of components to return (this is not a rotation, just an
#' \emph{a priori} selection of how much data should be returned).
#' @return Returns a large list of items which are also returned in
#' \code{\link{epPCA}} (the help files for those functions will refer to this
#' as well).\cr All items with a letter followed by an \emph{i} are for the
#' \emph{I} rows of a DATA matrix. All items with a letter followed by an
#' \emph{j} are for the \emph{J} rows of a DATA matrix.\cr\cr \item{fi}{factor
#' scores for the row items.} \item{di}{square distances of the row items.}
#' \item{ci}{contributions (to the variance) of the row items.}
#' \item{ri}{cosines of the row items.} \item{fj}{factor scores for the column
#' items.} \item{dj}{square distances of the column items.}
#' \item{cj}{contributions (to the variance) of the column items.}
#' \item{rj}{cosines of the column items.} \item{t}{the percent of explained
#' variance per component (tau).} \item{eigs}{the eigenvalues from the
#' decomposition.} \item{pdq}{the set of left singular vectors (pdq$p) for the
#' rows, singular values (pdq$Dv and pdq$Dd), and the set of right singular
#' vectors (pdq$q) for the columns.} \item{X}{the final matrix that was
#' decomposed (includes scaling, centering, masses, etc...).}
#' @author Derek Beaton and Hervé Abdi.
#' @seealso \code{\link{epPCA}}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H. (2007). Singular Value Decomposition (SVD) and
#' Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.):
#' \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage.
#' pp. 907-912.
#' @keywords misc multivariate
#' @export corePCA
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
