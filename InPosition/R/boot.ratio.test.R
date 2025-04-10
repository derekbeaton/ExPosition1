#' Performs bootstrap ratio test.
#' 
#' Performs bootstrap ratio test which is analogous to a \emph{t}- or
#' \emph{z}-score.
#' 
#' 
#' @param boot.cube an \code{\link{array}}. This is the bootstrap resampled
#' data. dim 1 (rows) are the items to be tested (e.g., \code{fj}, see
#' \code{\link{boot.compute.fj}}). dim 2 (columns) are the components from the
#' supplemental projection. dim 3 (depth) are each bootstrap sample.
#' @param critical.value numeric. This is the value that would be used as a
#' cutoff in a \emph{t}- or \emph{z}-test. Default is 2 (i.e., 1.96 rounded
#' up). The higher the number, the more difficult to reject the null.
#' @return A list with the following items:\cr
#' return(list(sig.boot.ratios=significant.boot.ratios,boot.ratios=boot.ratios,critical.value=critical.value))
#' \item{sig.boot.ratios}{This is a matrix with the same number of rows and
#' columns as \code{boot.cube}. If TRUE, the bootstrap ratio was larger than
#' \code{critical.value}. If FALSE, it was smaller.} \item{boot.ratios}{This is
#' a matrix with bootstrap ratio values that has the same number of rows and
#' columns as \code{boot.cube}.} \item{critical.value}{the critical value input
#' is also returned.}
#' @author Derek Beaton and Hervé Abdi
#' @seealso \code{\link{boot.compute.fj}}
#' @references The name bootstrap ratio comes from the Partial Least Squares in
#' Neuroimaging literature. See:\cr McIntosh, A. R., & Lobaugh, N. J. (2004).
#' Partial least squares analysis of neuroimaging data: applications and
#' advances. \emph{Neuroimage}, \emph{23}, S250--S263.\cr\cr The bootstrap
#' ratio is related to other tests of values with respect to the bootstrap
#' distribution, such as the Interval-\emph{t}. See:\cr Chernick, M. R. (2008).
#' \emph{Bootstrap methods: A guide for practitioners and researchers} (Vol.
#' 619). Wiley-Interscience.\cr Hesterberg, T. (2011). Bootstrap. \emph{Wiley
#' Interdisciplinary Reviews: Computational Statistics}, \emph{3}, 497–526. \cr
#' @export boot.ratio.test
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
#' 	#now compute the bootstrap ratios:
#' 	ratios.unconstrained <- boot.ratio.test(boot.fjs.unconstrained)
#' 	ratios.constrained <- boot.ratio.test(boot.fjs.constrained)
#' 
boot.ratio.test <- function(boot.cube,critical.value=2){	
	boot.cube.mean <- apply(boot.cube,c(1,2),mean)
	boot.cube.mean_repeat <- array(boot.cube.mean,dim=c(dim(boot.cube)))
	boot.cube.dev <- (boot.cube - boot.cube.mean_repeat)^2
	#s.boot<-(apply(boot.cube.dev,c(1,2),mean))^(1/2)
	s.boot<-sqrt(apply(boot.cube.dev,c(1,2),mean))
	boot.ratios <- boot.cube.mean / s.boot
	boot.ratios <-replace(boot.ratios,is.infinite(boot.ratios),0)
	significant.boot.ratios <- (abs(boot.ratios) > critical.value)
	rownames(boot.ratios) <- rownames(boot.cube)
	rownames(significant.boot.ratios) <- rownames(boot.cube)	
	return(list(sig.boot.ratios=significant.boot.ratios,boot.ratios=boot.ratios,critical.value=critical.value))
}
