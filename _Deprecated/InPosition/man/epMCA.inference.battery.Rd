\name{epMCA.inference.battery}
\alias{epMCA.inference.battery}
\encoding{UTF-8}
\title{
epMCA.inference.battery: Inference tests for Multiple Correspondence Analysis (CA) via InPosition.
}
\description{
Multiple Correspondence Analysis (CA) and a battery of inference tests via InPosition. The battery includes permutation and bootstrap tests.
}
\usage{
epMCA.inference.battery(DATA, make_data_nominal = TRUE, DESIGN = NULL, 
	make_design_nominal = TRUE, masses = NULL, weights = NULL, 
	hellinger = FALSE, symmetric = TRUE, correction = c("b"), 
	graphs = TRUE, k = 0, 
	test.iters = 100, constrained = FALSE, critical.value = 2)
}
\arguments{
	\item{DATA}{original data to perform a MCA on. This data can be in original formatting (qualitative levels) or in dummy-coded variables.}
  \item{make_data_nominal}{a boolean. If TRUE (default), DATA is recoded as a dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{masses}{a diagonal matrix or column-vector of masses for the row items.}
  \item{weights}{a diagonal matrix or column-vector of weights for the column it}
  \item{hellinger}{a boolean. If FALSE (default), Chi-square distance will be used. If TRUE, Hellinger distance will be used.}
  \item{symmetric}{a boolean. If TRUE symmetric factor scores for rows.}
  \item{correction}{which corrections should be applied? "b" = Benzécri correction, "bg" = Greenacre adjustment to Benzécri correction.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
  \item{test.iters}{number of iterations}
  \item{constrained}{a boolean. If a DESIGN matrix is used, this will constrain bootstrap resampling to be within groups.}
  \item{critical.value}{numeric. A value, analogous to a z- or t-score to be used to determine significance (via bootstrap ratio).}
}
\details{
	\code{epMCA.inference.battery} performs multiple correspondence analysis and inference tests on a data matrix. \cr\cr
	If the expected time to compute the results (based on \code{test.iters}) exceeds 1 minute, you will be asked (via command line) if you want to continue. 
}
\value{
Returns two lists ($Fixed.Data and $Inference.Data). For $Fixed.Data, see \code{\link{epMCA}}, \code{\link{coreCA}} for details on the descriptive (fixed-effects) results.

$Inference.Data returns:
\item{components}{Permutation tests of components. p-values ($p.vals) and distributions of eigenvalues ($eigs.perm) for each component}
\item{fj.boots}{Bootstrap tests of measures (columns). See \code{\link{boot.ratio.test}} output details.}
\item{omni}{Permutation tests of components. p-values ($p.val) and distributions of total inertia ($inertia.perm). This is only useful if \code{correction}s are performed. Total inertia is constant for permutation with no corrections in MCA.}
}
\author{
Derek Beaton, Joseph Dunlop, and Hervé Abdi.
}
\seealso{
	\code{\link{epMCA}}, \code{\link{epCA}}, \code{\link{epCA.inference.battery}}
}
\examples{
	data(mca.wine)
	mca.wine.res <- epMCA.inference.battery(mca.wine$data)
}
\keyword{ multivariate }
\keyword{ permutation }
\keyword{ bootstrap }