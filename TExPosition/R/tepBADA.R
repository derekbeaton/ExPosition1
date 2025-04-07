#' Barycentric Discriminant Analysis
#' 
#' Barycentric Discriminant Analysis (BADA) via TExPosition.
#' 
#' Note: BADA is a special case of PLS (\code{\link{tepPLS}}) wherein DATA1 are
#' data and DATA2 are a group-coded disjunctive matrix. This is also called
#' mean-centered PLS (Krishnan et al., 2011).
#' 
#' @param DATA original data to perform a BADA on.
#' @param scale a boolean, vector, or string. See \code{\link[ExPosition]{expo.scale}} for
#' details.
#' @param center a boolean, vector, or string. See \code{\link[ExPosition]{expo.scale}} for
#' details.
#' @param DESIGN a design matrix to indicate if rows belong to groups. Required
#' for BADA.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{tepGraphs}})
#' @param k number of components to return.
#' @return See \code{\link[ExPosition]{corePCA}} for details on what is returned. In
#' addition to the values returned:\cr \item{fii}{factor scores computed for
#' supplemental observations} \item{dii}{squared distances for supplemental
#' observations} \item{rii}{cosines for supplemental observations}
#' \item{assign}{a list of assignment data. See \code{\link{fii2fi}} and
#' \code{\link{R2}}} \item{lx}{latent variables from DATA1 computed for
#' observations} \item{ly}{latent variables from DATA2 computed for
#' observations}
#' @author Derek Beaton
#' @seealso \code{\link[ExPosition]{corePCA}}, \code{\link[ExPosition]{epPCA}}, \code{\link[ExPosition]{epMDS}}\cr
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H. and Williams, L.J. (2010). Correspondence analysis.
#' In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr Abdi, H. (2007).
#' Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr Abdi,
#' H. & Williams, L.J. (2010). Barycentric discriminant analysis (BADIA). In
#' N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 64-75. \cr Abdi, H.,
#' Williams, L.J., Beaton, D., Posamentier, M., Harris, T.S., Krishnan, A., &
#' Devous, M.D. (in press, 2012). Analysis of regional cerebral blood flow data
#' to discriminate among Alzheimer's disease, fronto-temporal dementia, and
#' elderly controls: A multi-block barycentric discriminant analysis (MUBADA)
#' methodology. \emph{Journal of Alzheimer Disease}, , -. Abdi, H., Williams,
#' L.J., Connolly, A.C., Gobbini, M.I., Dunlop, J.P., & Haxby, J.V. (2012).
#' Multiple Subject Barycentric Discriminant Analysis (MUSUBADA): How to assign
#' scans to categories without using spatial normalization. \emph{Computational
#' and Mathematical Methods in Medicine}, 2012, 1-15.
#' doi:10.1155/2012/634165.\cr Krishnan, A., Williams, L. J., McIntosh, A. R.,
#' & Abdi, H. (2011). Partial Least Squares (PLS) methods for neuroimaging: A
#' tutorial and review. \emph{NeuroImage}, \emph{56}(\bold{2}), 455 -- 475.\cr
#' @keywords multivariate
#' @examples
#' 
#' data(bada.wine)
#' bada.res <- tepBADA(bada.wine$data,scale=FALSE,DESIGN=bada.wine$design,make_design_nominal=FALSE)
#' 
#' @export tepBADA
tepBADA <- function(DATA,scale=TRUE,center=TRUE,DESIGN=NULL,make_design_nominal=TRUE,graphs=TRUE,k=0){	
		
	
	DESIGN <- texpoDesignCheck(DATA,DESIGN,make_design_nominal,force_bary=TRUE)
	colDESIGN <- colnames(DESIGN)
	massedDESIGN <- t(apply(DESIGN,1,'/',colSums(DESIGN)))
	colnames(massedDESIGN) <- colDESIGN	
	
	main <- deparse(substitute(DATA))		
	
	DATA <- expo.scale(as.matrix(DATA),scale=scale,center=center)
  R <- t(massedDESIGN) %*% DATA
	
	this.center <- attributes(DATA)$`scaled:center`
	this.scale <- attributes(DATA)$`scaled:scale`	

	colnames(R) <- colnames(DATA)
	rownames(R) <- colnames(DESIGN)	
	
	res <- epPCA(R, scale = FALSE, center = FALSE, graphs = FALSE, k = k)
	res <- res$ExPosition.Data


	supplementaryRes <- supplementaryRows(DATA,res)
	res$fii <- supplementaryRes$fii
	res$dii <- supplementaryRes$dii
	res$rii <- supplementaryRes$rii
	
	res$lx <- res$fii
	res$ly <- supplementaryCols(t(massedDESIGN),res,center=FALSE,scale=FALSE)$fjj

	assignments <- fii2fi(DESIGN,res$fii,res$fi)
	assignments$r2 <- R2(NULL,res$di,ind.masses=NULL,res$dii)
	class(assignments) <- c("tepAssign","list")
	res$assign <- assignments

	res$center <- this.center
	res$scale <- this.scale
	
	#new res here
	class(res) <- c("tepBADA","list")		
	tepPlotInfo <- tepGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs,lvPlots=FALSE)
	return(tepOutputHandler(res=res,tepPlotInfo=tepPlotInfo))
}
