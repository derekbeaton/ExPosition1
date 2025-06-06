#' Bootstrap computations for TInPosition.
#' 
#' Provides bootstrap projections for $fi and $fj from TExPosition methods.
#' 
#' 
#' @param DATA The original data matrix to be bootstrapped. Rows will be
#' bootstrapped and are assumed to be observations. Resampling will be
#' constrained to within groups based on \code{DESIGN}.
#' @param DESIGN A design matrix (in disjunctive coding). Required for
#' \code{TExPosition} and \code{TInPosition} analyses.
#' @param res of class \code{texpoOutput}. Results from one of the
#' \code{\link[TExPosition]{TExPosition}} methods (e.g., \code{\link[TExPosition]{tepDICA}},
#' \code{\link[TExPosition]{tepBADA}}),
#' @return \item{FBX}{a set of factor scores of the measures (columns,
#' \code{$fj}) for the bootstrapped data.} \item{FBY}{a set of factor scores of
#' the groups (\code{$fi}) for the bootstrapped data.}
#' @author Derek Beaton
#' @keywords bootstrap
#' @export boot.compute.fi.fj
boot.compute.fi.fj <- function(DATA,DESIGN,res){

	pca.types <- c('tepBADA')
	ca.types <- c('tepDICA')
	
	#A simple override/check. If someone puts in texpoOutput class data, tepGraphs will recognize it.
	tepPlotInfo <- NULL
	if(class(res)[1] == "texpoOutput"){
		if(length(res)==2){
			tepPlotInfo <- res$Plotting.Data
		}
		res <- res$TExPosition.Data
	}else{
		stop("Unknown class") ##fix this later with more checks.
	}
	
	boot.sample.vector <- boot.samples(DATA,DESIGN,constrained=TRUE)	
	BootX <- DATA[boot.sample.vector,]	
	
	
	if((class(res)[1] %in% c(pca.types))){
		massedDESIGN <- t(t(DESIGN) * (1/(colSums(DESIGN))))		
		BootY <- massedDESIGN[boot.sample.vector,]
	}
	if((class(res)[1] %in% c(ca.types))){
		BootY <- DESIGN[boot.sample.vector,]		
	}	
	Rboot<-t(BootY) %*% BootX
	
	
	Fboot.Y <- supplementaryRows(Rboot,res)$fii
	if((class(res)[1] %in% c(pca.types))){
		Fboot.X <- supplementaryCols(Rboot,res,center=res$center,scale=res$scale)$fjj
	}else if((class(res)[1] %in% c(ca.types))){
		Fboot.X <- supplementaryCols(Rboot,res)$fjj		
	}

	Fboot.Y <- replace(Fboot.Y,is.nan(Fboot.Y),0)
	Fboot.X <- replace(Fboot.X,is.nan(Fboot.X),0)	
	return(list(FBX=Fboot.X,FBY=Fboot.Y))
}

