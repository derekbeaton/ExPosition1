#' epCA: Correspondence Analysis (CA) via ExPosition.
#' 
#' Correspondence Analysis (CA) via ExPosition.
#' 
#' \code{epCA} performs correspondence analysis. Essentially, a PCA for
#' qualitative data (frequencies, proportions). If you decide to use Hellinger
#' distance, it is best to set \code{symmetric} to FALSE.
#' 
#' @usage epCA(DATA, DESIGN = NULL, make_design_nominal = TRUE, masses = NULL,
#' weights = NULL, hellinger = FALSE, symmetric = TRUE, graphs = TRUE, k = 0)
#' @param DATA original data to perform a CA on.
#' @param DESIGN a design matrix to indicate if rows belong to groups.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param masses a diagonal matrix or column-vector of masses for the row
#' items.
#' @param weights a diagonal matrix or column-vector of weights for the column
#' it
#' @param hellinger a boolean. If FALSE (default), Chi-square distance will be
#' used. If TRUE, Hellinger distance will be used.
#' @param symmetric a boolean. If TRUE (default) symmetric factor scores for
#' rows and columns are computed. If FALSE, the simplex (column-based) will be
#' returned.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{epGraphs}})
#' @param k number of components to return.
#' @return See \code{\link{coreCA}} for details on what is returned.
#' @author Derek Beaton
#' @seealso \code{\link{coreCA}}, \code{\link{epMCA}}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H., and Williams, L.J. (2010). Correspondence analysis.
#' In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr Abdi, H. (2007).
#' Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
#' Greenacre, M. J. (2007). Correspondence Analysis in Practice. \emph{Chapman
#' and Hall}.
#' @keywords multivariate
#' @examples
#' 
#' 	data(authors)
#' 	ca.authors.res <- epCA(authors$ca$data)
#' 
#' @export epCA
epCA <-
function(DATA,DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,weights=NULL,hellinger=FALSE,symmetric=TRUE,graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))
	DATA <- as.matrix(DATA)
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)
	
	res <- coreCA(DATA,masses=masses,weights=weights,hellinger=hellinger,symmetric=symmetric,k=k)
	class(res) <- c("epCA","list")
		
	epPlotInfo <- epGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)
	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
