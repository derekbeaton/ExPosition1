#tepDICA.inference.battery <- function(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, ind.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2){
#tepDICA.inference.battery <- function(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, group.masses = NULL, weights = NULL, symmetric = TRUE, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2){


#' Discriminant Correspondence Analysis Inference Battery
#' 
#' Discriminant Correspondence Analysis (DICA) Inference Battery via
#' TInPosition
#' 
#' \code{tepDICA.inference.battery} performs discriminant correspondence
#' analysis and inference tests on based on data and (row) design matrices.
#' \cr\cr If the expected time to compute the results (based on
#' \code{test.iters}) exceeds 1 minute, you will be asked (via command line) if
#' you want to continue.
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
#' (via \code{\link[ExPosition]{epGraphs}})
#' @param k number of components to return.
#' @param test.iters number of iterations
#' @param critical.value numeric. A value, analogous to a z- or t-score to be
#' used to determine significance (via bootstrap ratio).
#' @return Returns two lists ($Fixed.Data and $Inference.Data). For
#' $Fixed.Data, see \code{\link[TExPosition]{tepDICA}} and \code{\link[ExPosition]{coreCA}} for details
#' on the descriptive (fixed-effects) results.
#' 
#' $Inference.Data returns: \item{omni}{Permutation tests of components.
#' p-values ($p.val) and distributions of total inertia ($inertia.perm)}
#' \item{r2}{Permutation tests of R-squared value. p-values ($p.val) and
#' distributions of R2s ($r2.perm)} \item{components}{Permutation tests of
#' components. p-values ($p.vals) and distributions of eigenvalues ($eigs.perm)
#' for each component} \item{boot.data}{Bootstrap tests for $fi and $fj.
#' Contains distributions. See also \code{\link[InPosition]{boot.ratio.test}} output
#' details.} \item{loo.data}{Leave one out cross-validation tests. Includes
#' assignments ($loo.assign), factor scores ($loo.fii), LOO and fixed confusion
#' matrices ($loo.confuse, $fixed.confuse), and accuracy ($loo.acc,
#' $fixed.acc)}
#' @author Derek Beaton, Jenny Rieck, Hervé Abdi
#' @keywords multivariate
#' @examples
#' 
#' 	data(dica.wine)
#' 	data<-dica.wine$data
#' 	design<-dica.wine$design
#' 	dica.res <- 
#' 		tepDICA.inference.battery(data,DESIGN=design,
#' 		make_design_nominal=FALSE,test.iters=50)
#' @export tepDICA.inference.battery
tepDICA.inference.battery <- function(DATA, make_data_nominal = FALSE, DESIGN = NULL, make_design_nominal = TRUE, symmetric = TRUE, graphs = TRUE, k = 0, test.iters = 100, critical.value = 2){	
	############################	
	###private functions for now
	#loo.test <- function(DATA, DESIGN, group.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE,k = k, i){
	loo.test <- function(DATA, DESIGN, symmetric = TRUE,k = k, i){		
		Xminus1 <- DATA[-i,]
		Yminus1 <- DESIGN[-i,]
		#DICAminus1 <- tepDICA(Xminus1,DESIGN=Yminus1,make_design_nominal=FALSE,make_data_nominal=FALSE,hellinger=hellinger,symmetric=symmetric,weights=weights,group.masses=group.masses,graphs=FALSE,k=k)
	DICAminus1 <- tepDICA(Xminus1,DESIGN=Yminus1,make_design_nominal=FALSE,make_data_nominal=FALSE,symmetric=symmetric,graphs=FALSE,k=k)		
		supX <- supplementaryRows(SUP.DATA=t(DATA[i,]), res=DICAminus1)
		assignSup <- fii2fi(DESIGN=t(DESIGN[i,]), fii=supX$fii, fi=DICAminus1$TExPosition.Data$fi)
		return(list(assignSup=assignSup,supX=supX))
	}
	
	#permute.tests <- function(DATA, DESIGN = NULL, group.masses = NULL, weights = NULL, hellinger = FALSE, symmetric = TRUE, k = 0){
	permute.tests <- function(DATA, DESIGN = NULL, symmetric = TRUE, k = 0){		
		
		PermDATA <- DATA[sample(nrow(DATA),nrow(DATA),FALSE),]
		#res.perm <- tepDICA(PermDATA,DESIGN=DESIGN,make_design_nominal=FALSE,make_data_nominal=FALSE,hellinger=hellinger,symmetric=symmetric,weights=weights,group.masses=group.masses,graphs=FALSE,k=k)
		res.perm <- tepDICA(PermDATA,DESIGN=DESIGN,make_design_nominal=FALSE,make_data_nominal=FALSE,symmetric=symmetric,graphs=FALSE,k=k)
		
		perm.r2 <- res.perm$TExPosition.Data$assign$r2
		perm.eigs <- res.perm$TExPosition.Data$eigs	
		perm.inertia <- sum(perm.eigs)
		return(list(perm.r2=perm.r2,perm.eigs=perm.eigs,perm.inertia=perm.inertia))
	}
	############################
	
	DATA <- as.matrix(DATA)
	if(make_data_nominal){
		DATA <- makeNominalData(DATA)
	}
	
	DESIGN <- as.matrix(DESIGN)
	if(make_design_nominal){
		DESIGN <- makeNominalData(DESIGN)
	}
	
	#fixed.res <- tepDICA(DATA=DATA, make_data_nominal = FALSE, DESIGN = DESIGN, make_design_nominal = FALSE, group.masses = group.masses, ind.masses = ind.masses, weights = weights, hellinger = hellinger, symmetric = symmetric, graphs = FALSE, k = k)
	fixed.res <- tepDICA(DATA=DATA, make_data_nominal = FALSE, DESIGN = DESIGN, make_design_nominal = FALSE, symmetric = symmetric, graphs = FALSE, k = k)

	n.rows <- nrow(DATA)
	resamp.iters <- max(n.rows,test.iters)
	
	##inf stuff
	ncomps <- fixed.res$TExPosition.Data$pdq$ng
	FBY <- array(0,dim=c(nrow(fixed.res$TExPosition.Data$X),ncomps,test.iters))
	FBX <- array(0,dim=c(ncol(fixed.res$TExPosition.Data$X),ncomps,test.iters))
	eigs.perm.matrix <- matrix(0,test.iters,ncomps)
	r2.perm <- inertia.perm <- matrix(0,test.iters,1)
	
	##loo stuff
	loo.assign <- matrix(0,n.rows,ncol(DESIGN))
	loo.fii <- matrix(0,nrow(DESIGN),ncomps)	
	
		
	#boot & perm test next
	pb <- txtProgressBar(1,test.iters,1,style=1)
	for(i in 1:resamp.iters){
		if(i==1){ ##begin the inference clock.
			inf.start.time <- proc.time()
		}
		
		if(i <= test.iters){
			boot.res <- boot.compute.fi.fj(DATA,DESIGN,fixed.res)
			FBX[,,i] <- boot.res$FBX
			FBY[,,i] <- boot.res$FBY
			#permute.res <- permute.tests(DATA=DATA, DESIGN = DESIGN, group.masses = group.masses, weights = weights, hellinger = hellinger, symmetric = symmetric, k = k)
			permute.res <- permute.tests(DATA=DATA, DESIGN = DESIGN, symmetric = symmetric, k = k)
			eigs.perm.matrix[i,] <- permute.res$perm.eigs
			r2.perm[i,] <- permute.res$perm.r2
			inertia.perm[i,] <- permute.res$perm.inertia
		}

		if(i == 1){ ##end the inference clock; begin the loo clock.
			transition.time <- proc.time()
			inf.cycle.time <- transition.time - inf.start.time
			loo.start.time <- transition.time
		}

		if(i <= n.rows){
			#loo.test.res <- loo.test(DATA=DATA, DESIGN = DESIGN, group.masses = group.masses, weights = weights, hellinger = hellinger, symmetric = symmetric, k = k, i)
			loo.test.res <- loo.test(DATA=DATA, DESIGN = DESIGN, symmetric = symmetric, k = k, i)
			loo.assign[i,] <- loo.test.res$assignSup$assignments
			loo.fii[i,] <- loo.test.res$supX$fii
		}
		
		if(i == 1){ ##end the loo clock and interact with user.
			loo.cycle.time <- proc.time() - loo.start.time
		
			if(!continueResampling((inf.cycle.time [1] * test.iters) + (loo.cycle.time[1]*n.rows))){
				##exit strategy.
				return(fixed.res)
			}
		}
		setTxtProgressBar(pb,i)		
	}
		
	rownames(FBX) <- colnames(DATA)
	rownames(FBY) <- colnames(DESIGN)
	x.boot.tests <- boot.ratio.test(FBX,critical.value)
	class(x.boot.tests) <- c("tinpoBootTests","list")
	fj.boot.data <- list(tests=x.boot.tests,boots=FBX)
	class(fj.boot.data) <- c("tinpoBoot","list")	
	y.boot.tests <- boot.ratio.test(FBY,critical.value)
	class(y.boot.tests) <- c("tinpoBootTests","list")	
	fi.boot.data <- list(tests=y.boot.tests,boots=FBY)
	class(fi.boot.data) <- c("tinpoBoot","list")
	boot.data <- list(fj.boot.data=fj.boot.data,fi.boot.data=fi.boot.data)
	class(boot.data) <- c("tinpoAllBoots","list")	
	
	eigs.perm.matrix <- round(eigs.perm.matrix,digits=15)
	component.p.vals <- 1-(colSums(eigs.perm.matrix < matrix(fixed.res$TExPosition.Data$eigs,test.iters, ncomps,byrow=TRUE))/test.iters)
	component.p.vals[which(component.p.vals==0)] <- 1/test.iters
	components.data <- list(p.vals=round(component.p.vals,digits=4), eigs.perm=eigs.perm.matrix, eigs=fixed.res$TExPosition.Data$eigs)
	class(components.data) <- c("tinpoComponents","list")
	
	omni.p <- max(1-(sum(inertia.perm < sum(fixed.res$TExPosition.Data$eigs))/test.iters),1/test.iters)
	omni.data <- list(p.val=round(omni.p,digits=4),inertia.perm=inertia.perm,inertia=sum(fixed.res$TExPosition.Data$eigs))
	class(omni.data) <- c("tinpoOmni","list")	
	
	r2.p <- max(1-(sum(r2.perm < sum(fixed.res$TExPosition.Data$assign$r2))/test.iters),1/test.iters)
	r2.data <- list(p.val=round(r2.p,digits=4),r2.perm=r2.perm,r2=fixed.res$TExPosition.Data$assign$r2)
	class(r2.data) <- c("tinpoR2","list")		
	
	loo.confuse <- t(loo.assign) %*% DESIGN	
	rownames(loo.confuse) <- paste(colnames(DESIGN),"predicted",sep=".") 
	colnames(loo.confuse) <- paste(colnames(DESIGN),"actual",sep=".")
	fixed.confuse <- fixed.res$TExPosition.Data$assign$confusion
	loo.acc <- sum(diag(loo.confuse))/sum(loo.confuse)
	fixed.acc <- sum(diag(fixed.confuse))/sum(fixed.confuse)	
	loo.data <- list(loo.assign=loo.assign, loo.fii=loo.fii, loo.confuse=loo.confuse, fixed.confuse=fixed.confuse, loo.acc=loo.acc, fixed.acc=fixed.acc)
	class(loo.data) <- c("tinpoLOO","list")
	
 	Inference.Data <- list(omni=omni.data,r2=r2.data,components=components.data,boot.data=boot.data,loo.data=loo.data)
 	class(Inference.Data) <- c("tepDICA.inference.battery","list")
 	
 	ret.data <- list(Fixed.Data=fixed.res,Inference.Data=Inference.Data)
 	class(ret.data) <- c("tinpoOutput","list")
 	
	if(graphs){
		tinGraphs(ret.data)
	}
	
 	return(ret.data)
}
