#' epMDS: Multidimensional Scaling (MDS) via ExPosition.
#' 
#' Multidimensional Scaling (MDS) via ExPosition.
#' 
#' \code{epMDS} performs metric multi-dimensional scaling. Essentially, a PCA
#' for a symmetric distance matrix.
#' 
#' @usage epMDS(DATA, DATA_is_dist = TRUE, method="euclidean", DESIGN = NULL,
#' make_design_nominal = TRUE, masses = NULL, graphs = TRUE, k = 0)
#' @param DATA original data to perform a MDS on.
#' @param DATA_is_dist a boolean. If TRUE (default) the DATA matrix should be a
#' symmetric distance matrix. If FALSE, a Euclidean distance of row items will
#' be computed and used.
#' @param method which distance metric should be used. \code{method} matches
#' \code{\link{dist}}; Two additional distances are avaialble: "correlation"
#' and "chi2". For "chi2" see \code{\link{chi2Dist}}. Default is "euclidean".
#' @param DESIGN a design matrix to indicate if rows belong to groups.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param masses a diagonal matrix (or vector) that contains the masses (for
#' the row items).
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{epGraphs}})
#' @param k number of components to return.
#' @return See \code{\link{coreMDS}} for details on what is returned. epMDS
#' only returns values related to row items (e.g., fi, ci); no column data is
#' returned.  \item{D}{the distance matrix that was decomposed. In most cases,
#' it is returned as a squared distance.}
#' @note With respect to input of \code{DATA}, \code{epMDS} differs slightly
#' from other versions of multi-dimensional scaling. \cr If you provide a
#' rectangular matrix (e.g., observations x measures), \code{epMDS} will
#' compute a distance matrix and square it. \cr If you provide a distance
#' (dissimilarity) matrix, \code{epMDS} does not square it.
#' @author Derek Beaton
#' @seealso \code{\link{corePCA}}, \code{\link{epPCA}}
#' @references Abdi, H. (2007). Metric multidimensional scaling. In N.J.
#' Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand
#' Oaks (CA): Sage. pp. 598-605. \cr O'Toole, A. J., Jiang, F., Abdi, H., and
#' Haxby, J. V. (2005). Partially distributed representations of objects and
#' faces in ventral temporal cortex. \emph{Journal of Cognitive Neuroscience},
#' \emph{17}(4), 580-590.
#' @keywords multivariate
#' @examples
#' 	
#' 	data(jocn.2005.fmri)
#' 	#by default, components 1 and 2 will be plotted.
#' 	mds.res.images <- epMDS(jocn.2005.fmri$images$data)
#' 
#' 	##iris example
#' 	data(ep.iris)
#' 	iris.rectangular <- epMDS(ep.iris$data,DATA_is_dist=FALSE)
#' 	iris.euc.dist <- dist(ep.iris$data,upper=TRUE,diag=TRUE)
#' 	iris.sq.euc.dist <- as.matrix(iris.euc.dist^2)
#' 	iris.sq <- epMDS(iris.sq.euc.dist)
#' 
#' @export epMDS
epMDS <-
function(DATA,DATA_is_dist=TRUE,method="euclidean",DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))	
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	DATA <- as.matrix(DATA)
		
	if(DATA_is_dist && (nrow(DATA)==ncol(DATA))){
		D <- DATA
		MW <- computeMW(D,masses=masses)
	}else{
		#print('Creating distance matrix from DATA.')
		D.MW <- makeDistancesAndWeights(DATA,method=method,masses=masses)
		D <- D.MW$D #already squared from the above function.
		MW <- D.MW$MW		
	}
	#S <- mdsTransform(D,MW)
	#res <- coreMDS(S,MW$M,k=k)
	
	res <- coreMDS(D,MW$M,k=k)
	#overwrite res with half of res because it's MDS and we don't care
	res <- list(fi=res$fi,di=res$di,ci=res$ci,ri=res$ri,t=res$t,eigs=res$eigs,pdq=res$pdq,M=res$masses,X=res$X,D=D)
	class(res) <- c("epMDS","list")

	epPlotInfo <- epGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)
	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
