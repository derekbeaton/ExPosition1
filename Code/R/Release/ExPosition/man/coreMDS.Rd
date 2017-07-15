\name{coreMDS}
\alias{coreMDS}
\encoding{UTF-8}
\title{
coreMDS
}
\description{
coreMDS performs metric multidimensional scaling (MDS).
}
\usage{
coreMDS(DATA, masses = NULL, decomp.approach = 'svd', k = 0)
}
\arguments{
  \item{DATA}{original data to decompose and analyze via the singular value decomposition.}
  \item{masses}{a vector or diagonal matrix with masses for the rows (observations). If NULL, one is created.}
  \item{decomp.approach}{string. A switch for different decompositions (typically for speed). See \code{\link{pickSVD}}.}
  \item{k}{number of components to return (this is not a rotation, just an \emph{a priori} selection of how much data should be returned).}  
}
\details{
 \code{\link{epMDS}} should not be used directly unless you plan on writing extensions to ExPosition. See \code{\link{epMDS}}
}
\value{
Returns a large list of items which are also returned in \code{\link{epMDS}}.\cr
All items with a letter followed by an \emph{i} are for the \emph{I} rows of a DATA matrix. All items with a letter followed by an \emph{j} are for the \emph{J} rows of a DATA matrix.\cr\cr
\item{fi}{factor scores for the row items.}
\item{di}{square distances of the row items.}
\item{ci}{contributions (to the variance) of the row items.}
\item{ri}{cosines of the row items.}
\item{masses}{a column-vector or diagonal matrix of masses (for the rows)}
\item{t}{the percent of explained variance per component (tau).}
\item{eigs}{the eigenvalues from the decomposition.}
\item{pdq}{the set of left singular vectors (pdq$p) for the rows, singular values (pdq$Dv and pdq$Dd), and the set of right singular vectors (pdq$q) for the columns.}
\item{X}{the final matrix that was decomposed (includes scaling, centering, masses, etc...).}
}
\references{
Abdi, H. (2007). Metric multidimensional scaling. In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand Oaks (CA): Sage. pp. 598-605. \cr
O'Toole, A. J., Jiang, F., Abdi, H., and Haxby, J. V. (2005). Partially distributed representations of objects and faces in ventral temporal cortex. \emph{Journal of Cognitive Neuroscience}, \emph{17}(4), 580-590.
}
\author{
Derek Beaton and Hervé Abdi.
}
\seealso{
	\code{\link{epMDS}}
}
\keyword{ multivariate }
