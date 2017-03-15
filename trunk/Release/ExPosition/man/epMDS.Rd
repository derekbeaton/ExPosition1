\name{epMDS}
\alias{epMDS}
\title{
epMDS: Multidimensional Scaling (MDS) via ExPosition. 
}
\description{
Multidimensional Scaling (MDS) via ExPosition.
}
\usage{
epMDS(DATA, DATA_is_dist = TRUE, method="euclidean", DESIGN = NULL, 
	make_design_nominal = TRUE, masses = NULL, graphs = TRUE, k = 0)
}
\arguments{
  \item{DATA}{original data to perform a MDS on.}
  \item{DATA_is_dist}{a boolean. If TRUE (default) the DATA matrix should be a symmetric distance matrix. If FALSE, a Euclidean distance of row items will be computed and used.}
  \item{method}{which distance metric should be used. \code{method} matches \code{\link{dist}}; Two additional distances are avaialble: "correlation" and "chi2". For "chi2" see \code{\link{chi2Dist}}. Default is "euclidean".}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{masses}{a diagonal matrix (or vector) that contains the masses (for the row items).}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
}
\note{
	With respect to input of \code{DATA}, \code{epMDS} differs slightly from other versions of multi-dimensional scaling. \cr
	If you provide a rectangular matrix (e.g., observations x measures), \code{epMDS} will compute a distance matrix and square it. \cr
	If you provide a distance (dissimilarity) matrix, \code{epMDS} does not square it.
}
\details{
	\code{epMDS} performs metric multi-dimensional scaling. Essentially, a PCA for a symmetric distance matrix. 
}
\value{
	See \code{\link{coreMDS}} for details on what is returned. epMDS only returns values related to row items (e.g., fi, ci); no column data is returned.
	\item{D}{the distance matrix that was decomposed. In most cases, it is returned as a squared distance.}
}
\references{
Abdi, H. (2007). Metric multidimensional scaling. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand Oaks (CA): Sage. pp. 598-605. \cr
O'Toole, A. J., Jiang, F., Abdi, H., and Haxby, J. V. (2005). Partially distributed representations of objects and faces in ventral temporal cortex. \emph{Journal of Cognitive Neuroscience}, \emph{17}(4), 580-590. 
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{corePCA}}, \code{\link{epPCA}}, \code{\link{epGPCA}}
}
\examples{	
	data(jocn.2005.fmri)
	#by default, components 1 and 2 will be plotted.
	mds.res.images <- epMDS(jocn.2005.fmri$images$data)

	##iris example
	data(ep.iris)
	iris.rectangular <- epMDS(ep.iris$data,DATA_is_dist=FALSE)
	iris.euc.dist <- dist(ep.iris$data,upper=TRUE,diag=TRUE)
	iris.sq.euc.dist <- as.matrix(iris.euc.dist^2)
	iris.sq <- epMDS(iris.sq.euc.dist)
}
\keyword{ multivariate }
