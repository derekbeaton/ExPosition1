####The DATA that comes in is dependent on method (e.g., PCA vs. CA)
	#In the case of PCA, send in the already normalized matrix?
	#In the case of CA, just let it do its thing.

#for now, this has to take in the full Fixed.Data results.


#' Compute bootstrap resampled \code{fj} as supplemental elements.
#' 
#' This function computes a bootstrap resampled set of data and projects
#' \code{fj} as supplemental elements.
#' 
#' 
#' @param DATA The original data matrix to be bootstrapped. Rows will be
#' bootstrapped and are assumed to be observations.
#' @param res of class \code{expoOutput}. Results from one of the
#' \code{\link{ExPosition}} methods (e.g., \code{\link{epPCA}},
#' \code{\link{epMCA}}),
#' @param DESIGN A design matrix (in disjunctive coding). Only used if
#' \code{constrained} is TRUE.
#' @param constrained a boolean. If TRUE, bootstrap resampling will occur
#' within groups as designated by the \code{DESIGN} matrix.
#' @return \item{fjj}{a set of factor scores of the measures (columns,
#' \code{fj}) for the bootstrapped data.}
#' @author Derek Beaton
#' @seealso See the functions \code{\link{supplementaryCols}} and
#' \code{link{boot.samples}}
#' @references Chernick, M. R. (2008). \emph{Bootstrap methods: A guide for
#' practitioners and researchers} (Vol. 619). Wiley-Interscience.\cr
#' Hesterberg, T. (2011). Bootstrap. \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics}, \emph{3}, 497–526. \cr
#' @keywords multivariate inference bootstrap
#' @examples
#' 
#' 	##the following code generates 100 bootstrap resampled 
#' 	##projections of the measures from the Iris data set.
#' 	data(ep.iris)
#' 	data <- ep.iris$data
#' 	design <- ep.iris$design
#' 	iris.pca <- epPCA(data,scale="SS1",DESIGN=design,make_design_nominal=FALSE)
#' 	boot.fjs.unconstrained <- array(0,dim=c(dim(iris.pca$ExPosition.Data$fj),100))
#' 	boot.fjs.constrained <- array(0,dim=c(dim(iris.pca$ExPosition.Data$fj),100))
#' 	for(i in 1:100){
#' 		#unconstrained means we resample any of the 150 flowers
#' 		boot.fjs.unconstrained[,,i] <- boot.compute.fj(ep.iris$data,iris.pca)
#' 		#constrained resamples within each of the 3 groups
#' 		boot.fjs.constrained[,,i] <- boot.compute.fj(data,iris.pca,design,TRUE)		
#' 	}
#' 
boot.compute.fj <- function(DATA,res,DESIGN=NULL,constrained=FALSE){
	boot.index <- boot.samples(DATA=DATA,DESIGN=DESIGN,constrained=constrained)	

	output.types <- c("expoOutput")#,"texpoOutput","mexpoOutput")
	data.types <- c("ExPosition.Data")#,"TExPosition.Data","MExPosition.Data")	
	mds.types <- c('epMDS')#can add DiSTATIS to this.
	pca.types <- c('epPCA')#,'tepBADA')
	ca.types <- c('epCA','epMCA')#,'tepDICA')
	
	#I can probably trim this back...
	if(class(res)[1] %in% output.types){
		indicator <- which(class(res)[1] %in% output.types)
		if(sum(names(res) %in% data.types)==1 && length(names(res))==2){
			if(output.types[indicator]=="expoOutput"){
				if(!(class(res$ExPosition.Data)[1] %in% "epCA")){
					res$ExPosition.Data$fi <- res$ExPosition.Data$fi[boot.index,]
				}
				res <- res$ExPosition.Data
			}else{
				stop(paste("class of expoOutput required.", class(res)[1], "entered as res",sep=" "))	
			}				
		}else{
			stop(paste("res class type is unknown:",names(res),sep=" "))
		}
	}
	
	###some recognition needs to happen here...
	if((class(res)[1] %in% "epCA")){		
		boot.sup.data <- contingency.data.break(DATA,boot=TRUE)
		boot.index <- NULL
	}
	else{	
		boot.sup.data <- DATA[boot.index,]
	}

	#this should catch that things are NULL and skip them.
	if((class(res)[1] %in% c(pca.types))){
		fjj <- supplementaryCols(boot.sup.data,res,center=res$center,scale=res$scale)$fjj
	}else if((class(res)[1] %in% c(ca.types))){
		fjj <- supplementaryCols(boot.sup.data,res)$fjj
	}else if((class(res)[1] %in% c(mds.types))){ #this is the same as rows. 
		#fjj <- supplementaryCols(boot.sup.data,res)$fjj
		stop("epMDS data cannot be bootstrapped at this time. Please use epPCA.inference.battery()")
	}
	fjj<-replace(fjj,is.nan(fjj),0)
	return(fjj)
}
