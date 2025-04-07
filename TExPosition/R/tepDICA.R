#' Discriminant Correspondence Analysis
#' 
#' Discriminant Correspondence Analysis (DICA) via TExPosition.
#' 
#' If you use Hellinger distance, it is best to set \code{symmetric} to
#' FALSE.\cr\cr Note: DICA is a special case of PLS-CA (\code{\link{tepPLSCA}})
#' wherein DATA1 are data and DATA2 are a group-coded disjunctive matrix.
#' 
#' @param DATA original data to perform a DICA on. Data can be contingency
#' (like CA) or categorical (like MCA).
#' @param make_data_nominal a boolean. If TRUE (default), DATA is recoded as a
#' dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.
#' @param DESIGN a design matrix to indicate if rows belong to groups. Required
#' for DICA.
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
#' \item{fii}{factor scores computed for supplemental observations}
#' \item{dii}{squared distances for supplemental observations}
#' \item{rii}{cosines for supplemental observations} \item{assign}{a list of
#' assignment data. See \code{\link{fii2fi}} and \code{\link{R2}}}
#' \item{lx}{latent variables from DATA1 computed for observations}
#' \item{ly}{latent variables from DATA2 computed for observations}
#' @author Derek Beaton, Hervé Abdi
#' @seealso \code{\link{coreCA}}, \code{\link{epCA}}, \code{\link{epMCA}} \cr
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H. and Williams, L.J. (2010). Correspondence analysis.
#' In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr Abdi, H. (2007).
#' Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr Abdi,
#' H. (2007). Discriminant correspondence analysis. In N.J. Salkind (Ed.):
#' \emph{Encyclopedia of Measurement and Statistics}. Thousand Oaks (CA): Sage.
#' pp. 270-275. \cr Pinkham, A.E., Sasson, N.J., Beaton, D., Abdi, H., Kohler,
#' C.G., Penn, D.L. (in press, 2012). Qualitatively distinct factors contribute
#' to elevated rates of paranoia in autism and schizophrenia. \emph{Journal of
#' Abnormal Psychology}, 121, -.\cr Williams, L.J., Abdi, H., French, R., &
#' Orange, J.B. (2010). A tutorial on Multi-Block Discriminant Correspondence
#' Analysis (MUDICA): A new method for analyzing discourse data from clinical
#' populations. \emph{Journal of Speech Language and Hearing Research}, 53,
#' 1372-1393. \cr Williams, L.J., Dunlop, J.P., & Abdi, H. (2012). Effect of
#' age on the variability in the production of text-based global inferences.
#' \emph{PLoS One}, 7(5): e36161. doi:10.1371/ journal.pone.0036161 (pp.1-9)
#' @keywords multivariate
#' @examples
#' 
#' data(dica.wine)
#' dica.res <- tepDICA(dica.wine$data,DESIGN=dica.wine$design,make_design_nominal=FALSE)
#' 
#' @export tepDICA
tepDICA <- function(DATA,make_data_nominal=FALSE,DESIGN=NULL,make_design_nominal=TRUE,symmetric=TRUE,graphs=TRUE,k=0){
	
		
	DESIGN <- texpoDesignCheck(DATA,DESIGN,make_design_nominal,force_bary=TRUE)	
	main <- deparse(substitute(DATA))	
	DATA <- as.matrix(DATA)
	if(make_data_nominal){
		DATA <- makeNominalData(DATA)
	}
	
	#Make the group x variable contingency table
	R <- t(DESIGN) %*% DATA
	colnames(R) <- colnames(DATA)
	rownames(R) <- colnames(DESIGN)	
	

	
	res <- epCA(R, hellinger = FALSE, symmetric = symmetric, graphs = FALSE,k=k)	
	res <- res$ExPosition.Data

	supplementaryRes <- supplementaryRows(DATA,res)
	res$fii <- supplementaryRes$fii
	res$dii <- supplementaryRes$dii
	res$rii <- supplementaryRes$rii		
	
	res$ly <- supplementaryCols(t(DESIGN),res)$fjj * (1/sqrt(sum(R)))
	res$lx <- res$fii * matrix(rowSums(DATA),nrow(res$fii),ncol(res$fii)) * (1/sqrt(sum(R)))
		
	assignments <- fii2fi(DESIGN,res$fii,res$fi)
	assignments$r2 <- R2(res$M,res$di,ind.masses=NULL,res$dii)
	class(assignments) <- c("tepAssign","list")
	res$assign <- assignments

	#new res here
	class(res) <- c("tepDICA","list")	
	tepPlotInfo <- tepGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs,lvPlots=FALSE)
	return(tepOutputHandler(res=res,tepPlotInfo=tepPlotInfo))
}
