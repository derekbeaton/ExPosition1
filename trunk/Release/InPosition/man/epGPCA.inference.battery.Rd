\name{epGPCA.inference.battery}
\alias{epGPCA.inference.battery}
\encoding{UTF-8}
\title{
epGPCA.inference.battery: Inference tests for Generalized Principal Component Analysis (PCA) via InPosition.
}
\description{
Generalized Principal Component Analysis (PCA) and a battery of inference tests via InPosition. The battery includes permutation and bootstrap tests.
}
\usage{
epGPCA.inference.battery(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, 
	make_design_nominal = TRUE, masses = NULL, weights = NULL, 
	graphs = TRUE, k = 0, 
	test.iters = 100, constrained = FALSE, critical.value = 2)
}
\arguments{
  \item{DATA}{original data to perform a PCA on.}
  \item{scale}{a boolean, vector, or string. See \code{\link{expo.scale}} for details.}
  \item{center}{a boolean, vector, or string. See \code{\link{expo.scale}} for details.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{masses}{a diagonal matrix or column-vector of masses for the row items.}
  \item{weights}{a diagonal matrix or column-vector of weights for the column items.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
  \item{test.iters}{number of iterations}
  \item{constrained}{a boolean. If a DESIGN matrix is used, this will constrain bootstrap resampling to be within groups.}
  \item{critical.value}{numeric. A value, analogous to a z- or t-score to be used to determine significance (via bootstrap ratio).}
}
\details{
	\code{epGPCA.inference.battery} performs generalized principal components analysis and inference tests on a data matrix. \cr\cr
	If the expected time to compute the results (based on \code{test.iters}) exceeds 1 minute, you will be asked (via command line) if you want to continue. 
}
\value{
Returns two lists ($Fixed.Data and $Inference.Data). For $Fixed.Data, see \code{\link{epGPCA}}, \code{\link{corePCA}} for details on the descriptive (fixed-effects) results.

$Inference.Data returns:
\item{components}{Permutation tests of components. p-values ($p.vals) and distributions of eigenvalues ($eigs.perm) for each component}
\item{fj.boots}{Bootstrap tests of measures (columns). See \code{\link{boot.ratio.test}} output details.}
}
\author{
Derek Beaton and Hervé Abdi.
}

\seealso{
	\code{\link{epGPCA}}, \code{\link{epPCA}}, \code{\link{epPCA.inference.battery}}
}

\examples{
	#this is for ExPosition's iris data
	data(ep.iris)
	data<-ep.iris$data
	design<-ep.iris$design
	gpca.iris.res <- epGPCA.inference.battery(data,DESIGN=design,make_design_nominal=FALSE)
}
\keyword{ multivariate }
\keyword{ permutation }
\keyword{ bootstrap }
