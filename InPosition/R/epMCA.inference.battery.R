###function to handle fixed & random (bootstrap) effects for epMCA	


#' epMCA.inference.battery: Inference tests for Multiple Correspondence
#' Analysis (CA) via InPosition.
#' 
#' Multiple Correspondence Analysis (CA) and a battery of inference tests via
#' InPosition. The battery includes permutation and bootstrap tests.
#' 
#' \code{epMCA.inference.battery} performs multiple correspondence analysis and
#' inference tests on a data matrix. \cr\cr If the expected time to compute the
#' results (based on \code{test.iters}) exceeds 1 minute, you will be asked
#' (via command line) if you want to continue.
#' 
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
#' @param test.iters number of iterations
#' @param constrained a boolean. If a DESIGN matrix is used, this will
#' constrain bootstrap resampling to be within groups.
#' @param critical.value numeric. A value, analogous to a z- or t-score to be
#' used to determine significance (via bootstrap ratio).
#' @return Returns two lists ($Fixed.Data and $Inference.Data). For
#' $Fixed.Data, see \code{\link{epMCA}}, \code{\link{coreCA}} for details on
#' the descriptive (fixed-effects) results.
#' 
#' $Inference.Data returns: \item{components}{Permutation tests of components.
#' p-values ($p.vals) and distributions of eigenvalues ($eigs.perm) for each
#' component} \item{fj.boots}{Bootstrap tests of measures (columns). See
#' \code{\link{boot.ratio.test}} output details.} \item{omni}{Permutation tests
#' of components. p-values ($p.val) and distributions of total inertia
#' ($inertia.perm). This is only useful if \code{correction}s are performed.
#' Total inertia is constant for permutation with no corrections in MCA.}
#' @author Derek Beaton, Joseph Dunlop, and Hervé Abdi.
#' @seealso \code{\link{epMCA}}, \code{\link{epCA}},
#' \code{\link{epCA.inference.battery}}
#' @keywords multivariate permutation bootstrap
#' @examples
#' 
#' 	data(mca.wine)
#' 	mca.wine.res <- epMCA.inference.battery(mca.wine$data)
#' 
epMCA.inference.battery <- function(DATA, make_data_nominal = TRUE, DESIGN = NULL, make_design_nominal = TRUE, masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, correction = c("b"), graphs = TRUE, k = 0, test.iters=100, constrained=FALSE, critical.value=2){

####private functions
permute.components.mca <- function(DATA,make_data_nominal=TRUE,hellinger=FALSE,symmetric=TRUE,masses=NULL,weights=NULL,correction=c("b"),k=0){
 	if(!make_data_nominal){
		DATA <- rebuildMCAtable(DATA)
		#make_data_nominal <- !make_data_nominal #does not matter; I am forcing TRUE
	}
	perm.DATA <- apply(DATA,2,sample)
	return(epMCA(perm.DATA,make_data_nominal=TRUE,hellinger=hellinger,symmetric=symmetric,graphs=FALSE,k=k,masses=masses,weights=weights,correction=correction)$ExPosition.Data$eigs)
}
####end private

	if(make_data_nominal){
		nom.DATA <- makeNominalData(DATA)
		#make_data_nominal <- !make_data_nominal
	}else{
		nom.DATA <- DATA	
	}

	fixed.res <- epMCA(nom.DATA, make_data_nominal=FALSE, DESIGN, make_design_nominal, masses, weights, hellinger, symmetric, correction, graphs=FALSE, k)
			
	ncomps <- fixed.res$ExPosition.Data$pdq$ng		
	fj.boot.array <- array(0,dim=c(ncol(nom.DATA),ncomps,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,min(dim(nom.DATA)))

	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:test.iters){
		if(i==1){
			start.time <- proc.time()
		}
		
		fj.boot.array[,,i] <- boot.compute.fj(nom.DATA,fixed.res,DESIGN,constrained)
		perm.eigs <- permute.components.mca(DATA,make_data_nominal=make_data_nominal,hellinger=hellinger,symmetric=symmetric,masses=masses,weights=weights,correction=correction,k=k)
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
	
	#rownames(fj.boot.array) <- colnames(nom.DATA)
	#fj.boot.data <- list(fj.tests=boot.ratio.test(fj.boot.array,critical.value=critical.value),fj.boots=fj.boot.array)
	#class(fj.boot.data) <- c("inpoBootstrap", "list")
	rownames(fj.boot.array) <- colnames(nom.DATA)
	boot.ratio.test.data <- boot.ratio.test(fj.boot.array,critical.value=critical.value)
	class(boot.ratio.test.data) <- c("inpoBootTests","list")
	fj.boot.data <- list(tests=boot.ratio.test.data,boots=fj.boot.array)
	class(fj.boot.data) <- c("inpoBoot", "list")	
	
	##do I still need this rounding?
	eigs.perm.matrix <- eigs.perm.matrix
	inertia.perm <- rowSums(round(eigs.perm.matrix,digits=15))
	fixed.inertia <- sum(round(fixed.res$ExPosition.Data$eigs,digits=15))
	omni.p <- max(1-(sum(inertia.perm < fixed.inertia)/test.iters),1/test.iters)
	omni.data <- list(p.val=round(omni.p,digits=4),inertia.perm=inertia.perm, inertia=fixed.inertia)
	class(omni.data) <- c("inpoOmni","list")
	
	eigs.perm.matrix <- eigs.perm.matrix[,1:ncomps]
	component.p.vals <- 1-(colSums(eigs.perm.matrix < matrix(fixed.res$ExPosition.Data$eigs,test.iters, ncomps,byrow=TRUE))/test.iters)
	component.p.vals[which(component.p.vals==0)] <- 1/test.iters
	components.data <- list(p.vals=round(component.p.vals,digits=4), eigs.perm=eigs.perm.matrix, eigs=fixed.res$ExPosition.Data$eigs)
	class(components.data) <- c("inpoComponents","list")	
	
 	Inference.Data <- list(components=components.data,fj.boots=fj.boot.data,omni=omni.data)
	class(Inference.Data) <- c("epMCA.inference.battery","list")

	ret.data <- list(Fixed.Data=fixed.res,Inference.Data=Inference.Data)
	class(ret.data) <- c("inpoOutput","list")	

	#graphing needs to happen here.
	if(graphs){
		inGraphs(ret.data)
	}

	return(ret.data)
	
}
