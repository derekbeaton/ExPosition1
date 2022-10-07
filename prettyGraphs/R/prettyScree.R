#prettyScree <- function(eigs,retain.col="mediumorchid4",dismiss.col="gray",n.comps=NULL,perc.exp=1.0,comp.labels=NULL,show.only.good=FALSE,full.lim=FALSE,main=""){


#' prettyScree
#' 
#' prettyScree creates simple, crisp, publication-style scree plots and
#' ``tests'' for SVD-based analyses.
#' 
#' prettyScree visualizes the distribution of eigenvalues/explained variance
#' from SVD-based analyses. Further, prettyScree performs several rudimentary
#' ``tests''. Two rudimentary criteria are implemented: 1) user selected
#' explained variance, and 2) user selected number of components. Additionally,
#' two rudimentary ``tests'' are implemented: 1) the ``broken-stick''
#' distribution of variance model, and 2) the ``Kaiser criterion'' where all
#' components that explain more variance than the mean are kept.
#' 
#' prettyScree colors components that ``pass'' all selected tests with
#' \code{retain.col}. Any components that do not pass tests are colored by a
#' more transparent version of \code{retain.col}. Components that do not meet
#' any criteria for retention are colored by \code{dismiss.col}.
#' 
#' prettyScree should be considered ``under development'' as of 12.09.2013. The
#' function works, but we will be adding more features in the near future.
#' 
#' @param eigs a vector of \emph{positive} eigenvalues.
#' @param retain.col a color for components that are kept.
#' @param dismiss.col a color for components that are dismissed.
#' @param perc.exp a value between 0 and 1. Used to retain components that
#' explain \code{perc.comp} * 100 variance. Note: perc.exp retains
#' \code{cumsum(explained variance) < (perc.exp * 100)} + 1 component.
#' @param n.comps a value between 1 and \code{length(eigs)}. Used to retain
#' \code{n.comps} number of components.
#' @param broken.stick a boolean. If TRUE (default), the broken-stick test is
#' performed (see details).
#' @param kaiser a boolean. If TRUE (default), all components with eigenvalues
#' greater than the \code{mean(eigs)} are retained.
#' @param main A title to be placed at the top of the graph.
#' @return \item{comps.tests}{a matrix with boolean values. Rows indicate which
#' criteria are used, and columns correspond to components. If TRUE, a
#' component is considered ``retained'' by that test/criteria. If FALSE, the
#' component is ``dismissed'' according to that test/criteria.}
#' @note A private function (\code{add.alpha}) was copied from
#' http://lamages.blogspot.com/2013/04/how-to-change-alpha-value-of-colours-in.html
#' @author Derek Beaton
#' @seealso Also see (some of) the other packages that perform a wide array of
#' SVD-based analyses: \code{ExPosition}, \code{FactoMineR}, \code{ca},
#' \code{ade4}
#' @references Cangelosi, R., & Goriely, A. (2007). Component retention in
#' principal component analysis with application to cDNA microarray data.
#' \emph{Biology direct}, \emph{2}(2), 1--21.\cr \cr Peres-Neto, P. R.,
#' Jackson, D. A., & Somers, K. M. (2005). How many principal components?
#' Stopping rules for determining the number of non-trivial axes revisited.
#' \emph{Computational Statistics & Data Analysis}, \emph{49}(4), 974--997. \cr
#' @keywords multivariate
#' @examples
#' 
#' 	prcomp.res <- prcomp(USArrests, scale = TRUE)
#' 	prettyScree(prcomp.res$sdev^2)
#' 	##or
#' 	princomp.res <- princomp(USArrests, cor = TRUE)	
#' 	prettyScree(princomp.res$sdev^2)
#' 
#' @export prettyScree
prettyScree <- function(eigs,retain.col="mediumorchid4",dismiss.col="gray",perc.exp=1.0,n.comps=NULL,broken.stick=TRUE,kaiser=TRUE,main=""){

	##stolen from http://lamages.blogspot.com/2013/04/how-to-change-alpha-value-of-colours-in.html
	add.alpha <- function(col, alpha=1){

	  apply(sapply(col, col2rgb)/255, 2, 
                     function(x) 
                       rgb(x[1], x[2], x[3], alpha=alpha))  
	}
	
	
	eig.length <- length(eigs)
	mean.eig <- mean(eigs)
	eigs.round <- round(eigs,digits=3)
	exp.var <- eigs/sum(eigs)*100
	exp.var.round <- round(exp.var,digits=2)

	# if(is.null(comp.labels)){
		# #comp.labels <- paste(exp.var
	# }
	if(is.null(n.comps)){
		n.comps <- eig.length
	}
	if(n.comps > eig.length || n.comps < 1){
		n.comps <- eig.length
	}
	
	
	perc.exp.comps <- cumsum(exp.var) < (perc.exp * 100)
	perc.exp.comps[head(which(!(perc.exp.comps)),n=1)] <- TRUE
	
	keep.n.comps <- rep(FALSE,eig.length)
	keep.n.comps[1:n.comps] <- rep(TRUE,length(1:n.comps))
	
	comps.tests <- rbind(perc.exp.comps,keep.n.comps)
	
	if(broken.stick){
		broken.stick.distribution <- unlist(lapply(X=1:eig.length,FUN=function(x,n){return(tail((cumsum(1/x:n))/n,n=1))},n=eig.length)) * 100
		broken.stick.comps <- exp.var > broken.stick.distribution
		broken.stick.comps[head(which(!broken.stick.comps),n=1):eig.length] <- rep(FALSE,length(head(which(!broken.stick.comps),n=1):eig.length))
		comps.tests <- rbind(comps.tests,broken.stick.comps)	
	}
	if(kaiser){
		kaiser.mean.comps <- eigs > mean.eig
		comps.tests <- rbind(comps.tests,kaiser.mean.comps)			
	}
	
	comp.sums <- colSums(comps.tests)
	alpha.map <- 1/abs((comp.sums-(nrow(comps.tests)+1)))
	color.map <- rep(retain.col,eig.length)
	for(i in 1:eig.length){color.map[i] <- add.alpha(color.map[i],alpha.map[i])}
	color.map[which(comp.sums==0)] <- rep(dismiss.col,sum(comp.sums==0))
	
	
	dev.new()
	par(mar=c(5, 5, 4, 5) + 0.1)
	these.sizes <- ((log(exp.var) + abs(min(log(exp.var))))/max(log(exp.var) + abs(min(log(exp.var))))+0.1) * 3
	plot(exp.var,axes=FALSE,ylab="",xlab="Components",type="l",main=main,ylim=c(-1,max(exp.var)))
	points(exp.var,cex= these.sizes,pch=20,col=dismiss.col)
	points(exp.var,cex= these.sizes,pch=21,bg=color.map)		
	box()
	axis(2,at= exp.var,labels=eigs.round,las=2,lwd=3,cex.axis=.65)
	mtext("Eigenvalues",2,line=4)
	axis(4,at= exp.var,labels=paste(exp.var.round,"%",sep=""),las=2,lwd=3,cex.axis=.65)
	mtext("Explained Variance",4,line=4)
	axis(1,at=1:length(exp.var),lwd=3)	

	#return(comps.tests)
	invisible(comps.tests)

}
