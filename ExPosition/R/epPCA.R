#' epPCA: Principal Component Analysis (PCA) via ExPosition.
#' 
#' Principal Component Analysis (PCA) via ExPosition.
#' 
#' \code{epPCA} performs principal components analysis on a data matrix.
#' 
#' @usage epPCA(DATA, scale = TRUE, center = TRUE, DESIGN = NULL,
#' make_design_nominal = TRUE, graphs = TRUE, k = 0)
#' @param DATA original data to perform a PCA on.
#' @param scale a boolean, vector, or string. See \code{\link{expo.scale}} for
#' details.
#' @param center a boolean, vector, or string. See \code{\link{expo.scale}} for
#' details.
#' @param DESIGN a design matrix to indicate if rows belong to groups.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{epGraphs}})
#' @param k number of components to return.
#' @return See \code{\link{corePCA}} for details on what is returned.
#' @author Derek Beaton
#' @seealso \code{\link{corePCA}}, \code{\link{epMDS}}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H. (2007). Singular Value Decomposition (SVD) and
#' Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.):
#' \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage.
#' pp. 907-912.
#' @keywords multivariate
#' @examples
#' 
#' 	data(words)
#' 	pca.words.res <- epPCA(words$data)
#' 
#' @export epPCA
epPCA <-
function(DATA,scale=TRUE,center=TRUE,DESIGN=NULL,make_design_nominal=TRUE,graphs=TRUE,k=0){

	main <- deparse(substitute(DATA))
	DESIGN <- designCheck(DATA,DESIGN,make_design_nominal)
	DATA <- as.matrix(DATA)
	DATA <- expo.scale(DATA,scale=scale,center=center)
	this.center <- attributes(DATA)$`scaled:center`
	this.scale <- attributes(DATA)$`scaled:scale`

	res <- corePCA(DATA,k=k)
	res$center <- this.center
	res$scale <- this.scale
	class(res) <- c("epPCA","list")
	
	epPlotInfo <- epGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)
	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
