\name{prettyScree}
\alias{prettyScree}
\title{
prettyScree
}
\description{
prettyScree creates simple, crisp, publication-style scree plots and ``tests'' for SVD-based analyses.
}
\usage{
prettyScree(eigs, retain.col = "mediumorchid4", dismiss.col = "gray", 
	perc.exp = 1, n.comps = NULL, broken.stick = TRUE, kaiser = TRUE, 
	main = "")
}
\arguments{
  \item{eigs}{a vector of \emph{positive} eigenvalues.}
  \item{retain.col}{a color for components that are kept.}
  \item{dismiss.col}{a color for components that are dismissed.}
  \item{perc.exp}{a value between 0 and 1. Used to retain components that explain \code{perc.comp} * 100 variance. Note: perc.exp retains \code{cumsum(explained variance) < (perc.exp * 100)} + 1 component.}
  \item{n.comps}{a value between 1 and \code{length(eigs)}. Used to retain \code{n.comps} number of components.}
  \item{broken.stick}{a boolean. If TRUE (default), the broken-stick test is performed (see details).}
  \item{kaiser}{a boolean. If TRUE (default), all components with eigenvalues greater than the \code{mean(eigs)} are retained.}
  \item{main}{A title to be placed at the top of the graph.}
}
\details{
prettyScree visualizes the distribution of eigenvalues/explained variance from SVD-based analyses. Further, prettyScree performs several rudimentary ``tests''. Two rudimentary criteria are implemented: 1) user selected explained variance, and 2) user selected number of components. Additionally, two rudimentary ``tests'' are implemented: 1) the ``broken-stick'' distribution of variance model, and 2) the ``Kaiser criterion'' where all components that explain more variance than the mean are kept.

prettyScree colors components that ``pass'' all selected tests with \code{retain.col}. Any components that do not pass tests are colored by a more transparent version of \code{retain.col}. Components that do not meet any criteria for retention are colored by \code{dismiss.col}.

prettyScree should be considered ``under development'' as of 12.09.2013. The function works, but we will be adding more features in the near future. 
}
\value{
	\item{comps.tests}{a matrix with boolean values. Rows indicate which criteria are used, and columns correspond to components. If TRUE, a component is considered ``retained'' by that test/criteria. If FALSE, the component is ``dismissed'' according to that test/criteria.}
}
\references{
Cangelosi, R., & Goriely, A. (2007). Component retention in principal component analysis with application to cDNA microarray data. \emph{Biology direct}, \emph{2}(2), 1--21.\cr \cr
Peres-Neto, P. R., Jackson, D. A., & Somers, K. M. (2005). How many principal components? Stopping rules for determining the number of non-trivial axes revisited. \emph{Computational Statistics & Data Analysis}, \emph{49}(4), 974--997.	\cr
}
\author{
Derek Beaton
}
\note{
	A private function (\code{add.alpha}) was copied from http://lamages.blogspot.com/2013/04/how-to-change-alpha-value-of-colours-in.html
}
\examples{
	prcomp.res <- prcomp(USArrests, scale = TRUE)
	prettyScree(prcomp.res$sdev^2)
	##or
	princomp.res <- princomp(USArrests, cor = TRUE)	
	prettyScree(princomp.res$sdev^2)
}
\seealso{
	Also see (some of) the other packages that perform a wide array of SVD-based analyses: \code{ExPosition}, \code{FactoMineR}, \code{ca}, \code{ade4}
}
\keyword{ multivariate }
