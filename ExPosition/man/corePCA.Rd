\name{corePCA}
\alias{corePCA}
\encoding{UTF-8}
\title{
corePCA
}
\description{
corePCA performs the core of principal components analysis (PCA), and related techniques.
}
\usage{
corePCA(DATA, M = NULL, W = NULL, decomp.approach = 'svd', k = 0)
}
\arguments{
  \item{DATA}{original data to decompose and analyze via the singular value decomposition.}
  \item{M}{a vector or diagonal matrix with masses for the rows (observations). If NULL, one is created or the plain SVD is used.}
  \item{W}{a vector or diagonal matrix with weights for the columns (measures). If NULL, one is created or the plain SVD is used.}
  \item{decomp.approach}{string. A switch for different decompositions (typically for speed). See \code{\link{pickSVD}}.}  
  \item{k}{number of components to return (this is not a rotation, just an \emph{a priori} selection of how much data should be returned).}
}
\details{
This function should not be used directly. Please use \code{\link{epPCA}} unless you plan on writing extensions to ExPosition.
}
\value{
Returns a large list of items which are also returned in \code{\link{epPCA}} (the help files for those functions will refer to this as well).\cr
All items with a letter followed by an \emph{i} are for the \emph{I} rows of a DATA matrix. All items with a letter followed by an \emph{j} are for the \emph{J} rows of a DATA matrix.\cr\cr
\item{fi}{factor scores for the row items.}
\item{di}{square distances of the row items.}
\item{ci}{contributions (to the variance) of the row items.}
\item{ri}{cosines of the row items.}
\item{fj}{factor scores for the column items.}
\item{dj}{square distances of the column items.}
\item{cj}{contributions (to the variance) of the column items.}
\item{rj}{cosines of the column items.}
\item{t}{the percent of explained variance per component (tau).}
\item{eigs}{the eigenvalues from the decomposition.}
\item{pdq}{the set of left singular vectors (pdq$p) for the rows, singular values (pdq$Dv and pdq$Dd), and the set of right singular vectors (pdq$q) for the columns.}
\item{X}{the final matrix that was decomposed (includes scaling, centering, masses, etc...).}
}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.
}
\author{
Derek Beaton and Hervé Abdi.
}
\seealso{
	\code{\link{epPCA}}
}
\keyword{ misc }
\keyword{ multivariate }

