#' Supplemental projections.
#' 
#' Performs a supplementary projection across ExPosition (and related)
#' techniques.
#' 
#' 
#' @usage supplementalProjection(sup.transform = NULL, f.scores = NULL, Dv =
#' NULL, scale.factor = NULL, symmetric = TRUE)
#' @param sup.transform Data already transformed for supplementary projection.
#' That is, the output from: \code{\link{caSupplementalElementsPreProcessing}},
#' \code{\link{mdsSupplementalElementsPreProcessing}},
#' \code{\link{pcaSupplementaryColsPreProcessing}}, or
#' \code{\link{pcaSupplementaryRowsPreProcessing}}.
#' @param f.scores Active factor scores, e.g., res$ExPosition.Data$fi
#' @param Dv Active singular values, e.g., res$ExPosition.Data$pdq$Dv
#' @param scale.factor allows for a scaling factor of supplementary
#' projections. Primarily used for MCA supplemental projections to a correction
#' (e.g., Benzecri).
#' @param symmetric a boolean. Default is TRUE. If FALSE, factor scores are
#' computed with asymmetric properties (for rows only).
#' @return A list with: \cr \item{f.out}{Supplementary factor scores.}
#' \item{d.out}{Supplementary square distances.} \item{r.out}{Supplementary
#' cosines.}
#' @author Derek Beaton
#' @seealso It is preferred for users to compute supplemental projections via
#' \code{\link{supplementaryRows}} and \code{\link{supplementaryCols}}. These
#' handle some of the nuances and subtleties due to the different methods.
#' @keywords misc multivariate
#' @export supplementalProjection
supplementalProjection <- function(sup.transform=NULL,f.scores=NULL,Dv=NULL,scale.factor=NULL,symmetric=TRUE){
	if(is.null(sup.transform) || is.null(f.scores) || is.null(Dv)){
		stop('No inputs can be NULL.')
	}
	if(ncol(sup.transform)!=nrow(f.scores)){
		stop('Column dim of sup.transform does not match row dim of f.scores')
	}
	if(ncol(f.scores)!=length(Dv)){
		stop('Column dim of f.scores does not match length of Dv')
	}

	if(!symmetric){
		f.out <- sup.transform %*% f.scores	
	}else{
		f.out <- sup.transform %*% f.scores * matrix(Dv^-1,nrow(sup.transform),ncol(f.scores),byrow=TRUE)
	}
	if(!is.null(scale.factor)){		
		f.out <- f.out * matrix(scale.factor,nrow(f.out),ncol(f.scores),byrow=TRUE)
	}
	
	##can be replaced by the fcdr.helper
	f.out <- replace(f.out,is.nan(f.out),0)
	d.out <- rowSums(f.out^2)
	r.out <- matrix(1/d.out,nrow(f.out),ncol(f.out)) * (f.out^2)
	r.out <- replace(r.out,is.nan(r.out),0)
	d.out <- as.matrix(d.out)
	return(list(f.out=f.out,d.out=d.out,r.out=r.out))
}
