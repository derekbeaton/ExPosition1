#' epMCA: Multiple Correspondence Analysis (MCA) via ExPosition.
#' 
#' Multiple Correspondence Analysis (MCA) via ExPosition.
#' 
#' \code{epMCA} performs multiple correspondence analysis. Essentially, a CA
#' for categorical data. \cr It should be noted that when \code{hellinger} is
#' selected as TRUE, no correction will be performed. Additionally, if you
#' decide to use Hellinger, it is best to set \code{symmetric} to FALSE.
#' 
#' @usage epMCA(DATA, make_data_nominal = TRUE, DESIGN = NULL,
#' make_design_nominal = TRUE, masses = NULL, weights = NULL, hellinger =
#' FALSE, symmetric = TRUE, correction = c("b"), graphs = TRUE, k = 0)
#' @param DATA original data to perform a MCA on. This data can be in original
#' formatting (qualitative levels) or in dummy-coded variables.
#' @param make_data_nominal a boolean. If TRUE (default), DATA is recoded as a
#' dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.
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
#' @param symmetric a boolean. If TRUE symmetric factor scores for rows.
#' @param correction which corrections should be applied? "b" = Benzécri
#' correction, "bg" = Greenacre adjustment to Benzécri correction.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link{epGraphs}})
#' @param k number of components to return.
#' @return See \code{\link{coreCA}} for details on what is returned. In
#' addition to the values returned:\cr \item{$pdq}{this is the corrected SVD
#' data, if a correction was selected. If no correction was selected, it is
#' uncorrected.} \item{$pdq.uncor}{uncorrected SVD data.}
#' @author Derek Beaton
#' @seealso \code{\link{coreCA}}, \code{\link{epCA}},
#' \code{\link{mca.eigen.fix}}
#' @references Abdi, H., and Williams, L.J. (2010). Principal component
#' analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics},
#' 2, 433-459.\cr Abdi, H., and Williams, L.J. (2010). Correspondence analysis.
#' In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.): \emph{Encyclopedia of
#' Research Design}. Thousand Oaks (CA): Sage. pp. 267-278.\cr Abdi, H. (2007).
#' Singular Value Decomposition (SVD) and Generalized Singular Value
#' Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of
#' Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.\cr
#' Benzécri, J. P. (1979). Sur le calcul des taux d'inertie dans l'analyse d'un
#' questionnaire. \emph{Cahiers de l'Analyse des Données}, \bold{4},
#' 377-378.\cr Greenacre, M. J. (2007). Correspondence Analysis in Practice.
#' \emph{Chapman and Hall}.
#' @keywords multivariate
#' @examples
#' 
#' 	data(mca.wine)
#' 	mca.wine.res <- epMCA(mca.wine$data)
#' 
#' @export epMCA
epMCA <-
function(DATA,make_data_nominal=TRUE,DESIGN=NULL,make_design_nominal=TRUE,masses=NULL,weights=NULL,hellinger=FALSE,symmetric=TRUE,correction=c('b'),graphs=TRUE,k=0){
	main <- deparse(substitute(DATA))	
	DATA <- as.matrix(DATA)
	if(make_data_nominal){
		nominalData <- makeNominalData(DATA)
	}else{
		nominalData <- nominalCheck(DATA)
	}
	
	DESIGN<-designCheck(DATA,DESIGN,make_design_nominal)

	res <- coreCA(nominalData,masses=masses,weights=weights,hellinger=hellinger,symmetric=symmetric,k=k)
	##if they give the wrong correction
	weCanCorrect <- ((('b' %in% correction) || ('g' %in% correction)) && !hellinger)
	##if the data table is nominal, the col & row Sums should be identical and have 1 unique element.
	numUniqueRows <- length(unique(round(rowSums(nominalData))))
	if( weCanCorrect && (numUniqueRows==1) ){
		res <- mca.eigen.fix(nominalData, res, make_data_nominal=FALSE,correction=correction,symmetric=symmetric)
	}else{
		res$pdq.uncor <- res$pdq
	}
	class(res) <- c("epMCA","list")	
	
	epPlotInfo <- epGraphs(res=res,DESIGN=DESIGN,main=main,graphs=graphs)
	return(epOutputHandler(res=res,epPlotInfo=epPlotInfo))
}
