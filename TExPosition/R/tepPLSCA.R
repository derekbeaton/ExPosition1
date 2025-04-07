#Hellinger not available yet. Not until I can get the caNorm pipeline fixed.


#' Partial Least Squares-Correspondence Analysis
#' 
#' Partial Least Squares-Correspondence Analysis (PLSCA) via TExPosition.
#' 
#' This implementation of Partial Least Squares is for two categorical data
#' sets (Beaton et al., 2013), and based on the PLS method proposed by Tucker
#' (1958) and again by Bookstein (1994).
#' 
#' @param DATA1 Data matrix 1 (X), must be categorical (like MCA) or in
#' disjunctive code see \code{make_data1_nominal}.
#' @param DATA2 Data matrix 2 (Y), must be categorical (like MCA) or in
#' disjunctive code see \code{make_data2_nominal}.
#' @param make_data1_nominal a boolean. If TRUE (default), DATA1 is recoded as
#' a dummy-coded matrix. If FALSE, DATA1 is a dummy-coded matrix.
#' @param make_data2_nominal a boolean. If TRUE (default), DATA2 is recoded as
#' a dummy-coded matrix. If FALSE, DATA2 is a dummy-coded matrix.
#' @param DESIGN a design matrix to indicate if rows belong to groups.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param symmetric a boolean. If TRUE (default) symmetric factor scores for
#' rows.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{tepGraphs}})
#' @param k number of components to return.
#' @return See \code{\link{epCA}} (and also \code{\link{coreCA}}) for details
#' on what is returned. In addition to the values returned:\cr
#' \item{W1}{Weights for columns of DATA1, replaces \code{M} from
#' \code{coreCA}.} \item{W2}{Weights for columns of DATA2, replaces \code{W}
#' from \code{coreCA}.} \item{lx}{latent variables from DATA1 computed for
#' observations} \item{ly}{latent variables from DATA2 computed for
#' observations}
#' @author Derek Beaton, Hervé Abdi
#' @seealso \code{\link{coreCA}}, \code{\link{epCA}}, \code{\link{epMCA}},
#' \code{\link{tepDICA}} \cr
#' @references Tucker, L. R. (1958). An inter-battery method of factor
#' analysis. \emph{Psychometrika}, \emph{23}(2), 111--136. \cr Bookstein, F.,
#' (1994). Partial least squares: a dose–response model for measurement in the
#' behavioral and brain sciences. \emph{Psycoloquy} \emph{5} (23) \cr Abdi, H.
#' (2007). Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
#' Krishnan, A., Williams, L. J., McIntosh, A. R., & Abdi, H. (2011). Partial
#' Least Squares (PLS) methods for neuroimaging: A tutorial and review.
#' \emph{NeuroImage}, \emph{56}(\bold{2}), 455 -- 475.\cr Beaton, D., Filbey,
#' F., & Abdi H. (in press, 2013). Integrating partial least squares
#' correlation and correspondence analysis for nominal data. In Abdi, H., Chin,
#' W., Esposito Vinzi, V., Russolillo, G., & Trinchera, L. (Eds.), New
#' Perspectives in Partial Least Squares and Related Methods. New York:
#' Springer Verlag.
#' @keywords multivariate
#' @examples
#' 
#' 	data(snps.druguse)
#' 	plsca.res <- tepPLSCA(snps.druguse$DATA1,snps.druguse$DATA2,
#' 			make_data1_nominal=TRUE,make_data2_nominal=TRUE)
#' 
#' @export tepPLSCA
tepPLSCA <-
function(DATA1,DATA2,make_data1_nominal=FALSE,make_data2_nominal=FALSE,DESIGN=NULL,make_design_nominal=TRUE,symmetric=TRUE,graphs=TRUE,k=0){

	main <- paste("PLSCA: ",deparse(substitute(DATA1))," & ", deparse(substitute(DATA2)),sep="")		
	if(nrow(DATA1) != nrow(DATA2)){
		stop("DATA1 and DATA2 must have the same number of rows.")
	}
	if(make_data1_nominal){
		DATA1 <- makeNominalData(DATA1)
	}
	if(make_data2_nominal){
		DATA2 <- makeNominalData(DATA2)
	}	
	
	DESIGN <- texpoDesignCheck(DATA1,DESIGN,make_design_nominal)
	DESIGN <- texpoDesignCheck(DATA2,DESIGN,make_design_nominal=FALSE)	

	DATA1 <- as.matrix(DATA1)
	DATA2 <- as.matrix(DATA2)	
	R <- t(DATA1) %*% DATA2
	
	res <- coreCA(R,hellinger=FALSE,symmetric=symmetric,k=k)
	
	res$W1 <- res$M
	res$W2 <- res$W
	res$M <- res$W <- NULL
	
	res$lx <- supplementalProjection(makeRowProfiles(DATA1)$rowProfiles,res$fi,Dv=res$pdq$Dv)$f.out / sqrt(nrow(DATA1))
	if(symmetric){
		res$ly <- supplementalProjection(makeRowProfiles(DATA2)$rowProfiles,res$fj,Dv=res$pdq$Dv)$f.out / sqrt(nrow(DATA2))
	}else{
		res$ly <- (supplementalProjection(makeRowProfiles(DATA2)$rowProfiles,res$fj,Dv=rep(1,length(res$pdq$Dv)))$f.out / sqrt(nrow(DATA2)))
	}
	
	class(res) <- c("tepPLSCA","list")
	tepPlotInfo <- tepGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)	
	return(tepOutputHandler(res=res,tepPlotInfo=tepPlotInfo))
}
