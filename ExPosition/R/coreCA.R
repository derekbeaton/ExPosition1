#' coreCA
#' 
#' coreCA performs the core of correspondence analysis (CA), multiple
#' correspondence analysis (MCA) and related techniques.
#' 
#' This function should not be used directly. Please use \code{\link{epCA}} or
#' \code{\link{epMCA}} unless you plan on writing extensions to ExPosition. Any
#' extensions wherein CA is the primary analysis should use \code{coreCA}.
#' 
#' @usage coreCA(DATA, masses = NULL, weights = NULL, hellinger = FALSE,
#' symmetric = TRUE, decomp.approach = 'svd', k = 0)
#' @param DATA original data to decompose and analyze via the singular value
#' decomposition.
#' @param masses a vector or diagonal matrix with masses for the rows
#' (observations). If NULL, one is created or the plain SVD is used.
#' @param weights a vector or diagonal matrix with weights for the columns
#' (measures). If NULL, one is created or the plain SVD is used.
#' @param hellinger a boolean. If FALSE (default), Chi-square distance will be
#' used. If TRUE, Hellinger distance will be used.
#' @param symmetric a boolean. If TRUE (default) symmetric factor scores for
#' rows and columns are computed. If FALSE, the simplex (column-based) will be
#' returned.
#' @param decomp.approach string. A switch for different decompositions
#' (typically for speed). See \code{\link{pickSVD}}.
#' @param k number of components to return (this is not a rotation, just an
#' \emph{a priori} selection of how much data should be returned).
#' @return Returns a large list of items which are also returned in
#' \code{\link{epCA}} and \code{\link{epMCA}} (the help files for those
#' functions will refer to this as well).\cr All items with a letter followed
#' by an \emph{i} are for the \emph{I} rows of a DATA matrix. All items with a
#' letter followed by an \emph{j} are for the \emph{J} rows of a DATA
#' matrix.\cr\cr \item{fi}{factor scores for the row items.} \item{di}{square
#' distances of the row items.} \item{ci}{contributions (to the variance) of
#' the row items.} \item{ri}{cosines of the row items.} \item{fj}{factor scores
#' for the column items.} \item{dj}{square distances of the column items.}
#' \item{cj}{contributions (to the variance) of the column items.}
#' \item{rj}{cosines of the column items.} \item{t}{the percent of explained
#' variance per component (tau).} \item{eigs}{the eigenvalues from the
#' decomposition.} \item{pdq}{the set of left singular vectors (pdq$p) for the
#' rows, singular values (pdq$Dv and pdq$Dd), and the set of right singular
#' vectors (pdq$q) for the columns.} \item{M}{a column-vector or diagonal
#' matrix of masses (for the rows)} \item{W}{a column-vector or diagonal matrix
#' of weights (for the columns)} \item{c}{a centering vector (for the
#' columns).} \item{X}{the final matrix that was decomposed (includes scaling,
#' centering, masses, etc...).} \item{hellinger}{a boolean. TRUE if Hellinger
#' distance was used.} \item{symmetric}{a boolean. FALSE if asymmetric factor
#' scores should be computed.}
#' @author Derek Beaton and Hervé Abdi.
#' @seealso \code{\link{epCA}}, \code{\link{epMCA}}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H., and Williams, L.J. (2010). Correspondence analysis.
#' In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr Abdi, H. (2007).
#' Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.
#' Greenacre, M. J. (2007). Correspondence Analysis in Practice. \emph{Chapman
#' and Hall}.
#' @keywords misc multivariate
#' @export coreCA
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
	
	return(list(fi=fi,di=di,ci=ci,ri=ri,fj=fj,cj=cj,rj=rj,dj=dj,t=pdq_results$tau,eigs=pdq_results$eigs,M=mRP$masses,W=mRP$weights,c= mRP$rowCenter,pdq=pdq_results,X=mRP$deviations,hellinger=hellinger,symmetric=symmetric))
}
