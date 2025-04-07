#' Partial Least Squares
#' 
#' Partial Least Squares (PLS) via TExPosition.
#' 
#' This implementation of Partial Least Squares is a symmetric analysis. It was
#' first described by Tucker (1958), again by Bookstein (1994), and has gained
#' notoriety in Neuroimaging from McIntosh et al., (1996).
#' 
#' @param DATA1 Data matrix 1 (X)
#' @param DATA2 Data matrix 2 (Y)
#' @param center1 a boolean, vector, or string to center \code{DATA1}. See
#' \code{\link[ExPosition]{expo.scale}} for details.
#' @param scale1 a boolean, vector, or string to scale \code{DATA1}. See
#' \code{\link[ExPosition]{expo.scale}} for details.
#' @param center2 a boolean, vector, or string to center \code{DATA2}. See
#' \code{\link[ExPosition]{expo.scale}} for details.
#' @param scale2 a boolean, vector, or string to scale \code{DATA2}. See
#' \code{\link[ExPosition]{expo.scale}} for details.
#' @param DESIGN a design matrix to indicate if rows belong to groups.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{tepGraphs}})
#' @param k number of components to return.
#' @return See \code{\link{corePCA}} for details on what is returned. In
#' addition to the values returned:\cr \item{lx}{latent variables from DATA1
#' computed for observations} \item{ly}{latent variables from DATA2 computed
#' for observations} \item{data1.norm}{center and scale information for DATA1}
#' \item{data1.norm}{center and scale information for DATA2}
#' @author Derek Beaton
#' @seealso \code{\link{corePCA}}, \code{\link{epPCA}}, \code{\link{tepBADA}},
#' \code{\link{tepPLSCA}}\cr
#' @references Tucker, L. R. (1958). An inter-battery method of factor
#' analysis. \emph{Psychometrika}, \emph{23}(2), 111--136. \cr Bookstein, F.,
#' (1994). Partial least squares: a dose–response model for measurement in the
#' behavioral and brain sciences. \emph{Psycoloquy} \emph{5} (23) \cr McIntosh,
#' A. R., Bookstein, F. L., Haxby, J. V., & Grady, C. L. (1996). Spatial
#' Pattern Analysis of Functional Brain Images Using Partial Least Squares.
#' \emph{NeuroImage}, \emph{3}(3), 143--157. \cr
#' 
#' Krishnan, A., Williams, L. J., McIntosh, A. R., & Abdi, H. (2011). Partial
#' Least Squares (PLS) methods for neuroimaging: A tutorial and review.
#' \emph{NeuroImage}, \emph{56}(\bold{2}), 455 -- 475.\cr McIntosh, A. R., &
#' Lobaugh, N. J. (2004). Partial least squares analysis of neuroimaging data:
#' applications and advances. \emph{Neuroimage}, \emph{23}, S250--S263.\cr
#' @keywords multivariate
#' @examples
#' 
#' data(beer.tasting.notes)
#' data1<-beer.tasting.notes$data[,1:8]
#' data2<-beer.tasting.notes$data[,9:16]
#' pls.res <- tepPLS(data1,data2)
#' 
#' @export tepPLS
tepPLS <- function(DATA1,DATA2,center1=TRUE,scale1="SS1",center2=TRUE,scale2="SS1",DESIGN=NULL,make_design_nominal=TRUE,graphs=TRUE,k=0){

	if(nrow(DATA1) != nrow(DATA2)){
		stop("DATA1 and DATA2 must have the same number of rows.")
	}

	main <- paste("PLS: ",deparse(substitute(DATA1))," & ", deparse(substitute(DATA2)),sep="")
	DESIGN <- texpoDesignCheck(DATA1,DESIGN,make_design_nominal=make_design_nominal)
	DESIGN <- texpoDesignCheck(DATA2,DESIGN, make_design_nominal=FALSE)	

	DATA1 <- as.matrix(DATA1)
	DATA2 <- as.matrix(DATA2)	
	DATA1 <- expo.scale(DATA1,scale=scale1,center=center1)	
	DATA2 <- expo.scale(DATA2,scale=scale2,center=center2)		

	R <- t(DATA1) %*% DATA2

	#res <- corePCA(R,M=MW1$W,W=MW2$W,k=k)
	#res <- corePCA(R,k=k)
	res <- epPCA(DATA=R,k=k,graphs=FALSE,scale=FALSE,center=FALSE)
	res <- res$ExPosition.Data
	res$center <- NULL
	res$scale <- NULL	
	res$W1 <- res$M
	res$W2 <- res$W
	res$M <- res$W <- NULL
	res$data1.norm <- list(center=attributes(DATA1)$`scaled:center`,scale=attributes(DATA1)$`scaled:scale`)
	res$data2.norm <- list(center=attributes(DATA2)$`scaled:center`,scale=attributes(DATA2)$`scaled:scale`)	

	res$lx <- supplementalProjection(DATA1,res$fi,Dv=res$pdq$Dv)$f.out
	res$ly <- supplementalProjection(DATA2,res$fj,Dv=res$pdq$Dv)$f.out

	class(res) <- c("tepPLS","list")	
	tepPlotInfo <- tepGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)	
	return(tepOutputHandler(res=res,tepPlotInfo=tepPlotInfo))
}
