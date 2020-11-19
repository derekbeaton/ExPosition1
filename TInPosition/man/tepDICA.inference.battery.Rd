\name{tepDICA.inference.battery}
\alias{tepDICA.inference.battery}
\encoding{UTF-8}
\title{
Discriminant Correspondence Analysis Inference Battery
}
\description{
Discriminant Correspondence Analysis (DICA) Inference Battery via TInPosition
}
\usage{
tepDICA.inference.battery(DATA, make_data_nominal = FALSE, DESIGN = NULL, 
	make_design_nominal = TRUE,	
 	symmetric = TRUE, graphs = TRUE, k = 0,
	test.iters = 100, critical.value = 2)
}
\arguments{
\item{DATA}{original data to perform a DICA on. Data can be contingency (like CA) or categorical (like MCA).}
  \item{make_data_nominal}{a boolean. If TRUE (default), DATA is recoded as a dummy-coded matrix. If FALSE, DATA is a dummy-coded matrix.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups. Required for DICA.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{symmetric}{a boolean. If TRUE (default) symmetric factor scores for rows.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
  \item{test.iters}{number of iterations}
  \item{critical.value}{numeric. A value, analogous to a z- or t-score to be used to determine significance (via bootstrap ratio).}
}
\details{
	\code{tepDICA.inference.battery} performs discriminant correspondence analysis and inference tests on based on data and (row) design matrices. \cr\cr
	If the expected time to compute the results (based on \code{test.iters}) exceeds 1 minute, you will be asked (via command line) if you want to continue. 
}
\value{
Returns two lists ($Fixed.Data and $Inference.Data). For $Fixed.Data, see \code{\link{tepDICA}} and \code{\link{coreCA}} for details on the descriptive (fixed-effects) results.

$Inference.Data returns:
\item{omni}{Permutation tests of components. p-values ($p.val) and distributions of total inertia ($inertia.perm)}
\item{r2}{Permutation tests of R-squared value. p-values ($p.val) and distributions of R2s ($r2.perm)}
\item{components}{Permutation tests of components. p-values ($p.vals) and distributions of eigenvalues ($eigs.perm) for each component}
\item{boot.data}{Bootstrap tests for $fi and $fj. Contains distributions. See also \code{\link{boot.ratio.test}} output details.}
\item{loo.data}{Leave one out cross-validation tests. Includes assignments ($loo.assign), factor scores ($loo.fii),
	LOO and fixed confusion matrices ($loo.confuse, $fixed.confuse), and accuracy ($loo.acc, $fixed.acc)}
}

\author{
Derek Beaton, Jenny Rieck, Hervé Abdi
}
\examples{
	data(dica.wine)
	data<-dica.wine$data
	design<-dica.wine$design
	dica.res <- 
		tepDICA.inference.battery(data,DESIGN=design,
		make_design_nominal=FALSE,test.iters=50)
}
\keyword{ multivariate }