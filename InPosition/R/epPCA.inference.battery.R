#' epPCA.inference.battery: Inference tests for Principal Component Analysis
#' (PCA) via InPosition.
#' 
#' Principal Component Analysis (PCA) and a battery of inference tests via
#' InPosition. The battery includes permutation and bootstrap tests.
#' 
#' \code{epPCA.inference.battery} performs principal components analysis and
#' inference tests on a data matrix.\cr\cr If the expected time to compute the
#' results (based on \code{test.iters}) exceeds 1 minute, you will be asked
#' (via command line) if you want to continue.
#' 
#' @param DATA original data to perform a PCA on.
#' @param scale a boolean, vector, or string. See \code{\link[ExPosition]{expo.scale}} for
#' details.
#' @param center a boolean, vector, or string. See \code{\link[ExPosition]{expo.scale}} for
#' details.
#' @param DESIGN a design matrix to indicate if rows belong to groups.
#' @param make_design_nominal a boolean. If TRUE (default), DESIGN is a vector
#' that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a
#' dummy-coded matrix.
#' @param graphs a boolean. If TRUE (default), graphs and plots are provided
#' (via \code{\link[ExPosition]{epGraphs}})
#' @param k number of components to return.
#' @param test.iters number of iterations
#' @param constrained a boolean. If a DESIGN matrix is used, this will
#' constrain bootstrap resampling to be within groups.
#' @param critical.value numeric. A value, analogous to a z- or t-score to be
#' used to determine significance (via bootstrap ratio).
#' @return Returns two lists ($Fixed.Data and $Inference.Data). For
#' $Fixed.Data, see \code{\link[ExPosition]{epPCA}}, \code{\link[ExPosition]{corePCA}} for details on
#' the descriptive (fixed-effects) results.
#' 
#' $Inference.Data returns: \item{components}{Permutation tests of components.
#' p-values ($p.vals) and distributions of eigenvalues ($eigs.perm) for each
#' component} \item{fj.boots}{Bootstrap tests of measures (columns). See
#' \code{\link{boot.ratio.test}} output details.}
#' @author Derek Beaton and Hervé Abdi.
#' @seealso \code{\link[ExPosition]{epPCA}}
#' @keywords multivariate permutation bootstrap
#' @examples
#' 
#' 	data(words)
#' 	pca.words.res <- epPCA.inference.battery(words$data)
#' @export epPCA.inference.battery
epPCA.inference.battery <- function(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, graphs = TRUE, k = 0, test.iters=100, constrained=FALSE, critical.value=2){

###some private functions
permute.components.pca <- function(DATA,scale=TRUE,center=TRUE,k=0){
	perm.DATA <- apply(DATA,2,sample)
	return(epPCA(perm.DATA,graphs=FALSE,scale=scale,center=center,k=k)$ExPosition.Data$eigs)
}
###end private functions
    	
	fixed.res <- epPCA(DATA, scale, center, DESIGN, make_design_nominal, graphs=FALSE, k)

	ncomps <- fixed.res$ExPosition.Data$pdq$ng
	fj.boot.array <- array(0,dim=c(ncol(DATA),ncomps,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,min(dim(DATA)))
		
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		if(i==1){
			start.time <- proc.time()
		}
				
		fj.boot.array[,,i] <- boot.compute.fj(DATA,res=fixed.res,DESIGN=DESIGN,constrained=constrained)
		perm.eigs <- permute.components.pca(DATA,scale,center,k=k)
		eigs.perm.matrix[i,1:length(perm.eigs)] <- perm.eigs
		
		if(i==1){
			cycle.time <- (proc.time() - start.time) #this is in seconds...
			if(!continueResampling(cycle.time[1] * test.iters)){
				##exit strategy.
				return(fixed.res)
			}
		}
		setTxtProgressBar(pb,i)		
	}		

	#fj.boot.array <- replace(fj.boot.array,is.nan(fj.boot.array),0)
	rownames(fj.boot.array) <- colnames(DATA)
	boot.ratio.test.data <- boot.ratio.test(fj.boot.array,critical.value=critical.value)
	class(boot.ratio.test.data) <- c("inpoBootTests","list")
	fj.boot.data <- list(tests=boot.ratio.test.data,boots=fj.boot.array)
	class(fj.boot.data) <- c("inpoBoot", "list")
	
	eigs.perm.matrix <- eigs.perm.matrix[,1:ncomps]
	component.p.vals <- 1-(colSums(eigs.perm.matrix < matrix(fixed.res$ExPosition.Data$eigs,test.iters, ncomps,byrow=TRUE))/test.iters)
	component.p.vals[which(component.p.vals==0)] <- 1/test.iters
	components.data <- list(p.vals=round(component.p.vals,digits=4), eigs.perm=eigs.perm.matrix, eigs=fixed.res$ExPosition.Data$eigs)
	class(components.data) <- c("inpoComponents","list")
	
	Inference.Data <- list(components=components.data,fj.boots=fj.boot.data)
	class(Inference.Data) <- c("epPCA.inference.battery","list")

	ret.data <- list(Fixed.Data=fixed.res,Inference.Data=Inference.Data)
	class(ret.data) <- c("inpoOutput","list")	

	if(graphs){
		inGraphs(ret.data)
	}

	return(ret.data)
}
