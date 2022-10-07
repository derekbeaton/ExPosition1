#' Preprocessing for supplementary rows in Hellinger analyses.
#' 
#' Preprocessing for supplementary rows in Hellinger analyses.
#' 
#' 
#' @usage hellingerSupplementaryRowsPreProcessing(SUP.DATA, center = NULL)
#' @param SUP.DATA A supplemental matrix that has the same number of rows as an
#' active data set.
#' @param center The center from the active data. NULL will center
#' \code{SUP.DATA} to itself.
#' @return a matrix that has been preprocessed to project supplementary columns
#' for Hellinger methods.
#' @author Derek Beaton
#' @keywords misc multivariate
hellingerSupplementaryRowsPreProcessing <- function(SUP.DATA,center=NULL){
	
	if(is.null(center)){
		#stop("Hellinger supplementary rows require a center (from, e.g., active data)")
		print('No center for Hellinger. Computing center from SUP.DATA')
		hell.preproc <- makeRowProfiles(SUP.DATA,hellinger=TRUE)$deviations
	}
	else{
		hell.preproc <- rowNorms(SUP.DATA,type="hellinger")
		hell.preproc <- hell.preproc - matrix(center,nrow(SUP.DATA),ncol(SUP.DATA),byrow=TRUE)
	}
	
	#test.fi <- supplementalProjection(hell.preproc,f.scores=test.res$ExPosition.Data$fj,Dv=test.res$ExPosition.Data$pdq$Dv,symmetric=FALSE)$f.out

}
