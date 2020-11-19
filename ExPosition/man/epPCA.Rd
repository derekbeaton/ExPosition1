\name{epPCA}
\alias{epPCA}
\title{
epPCA: Principal Component Analysis (PCA) via ExPosition.
}
\description{
Principal Component Analysis (PCA) via ExPosition.
}
\usage{
epPCA(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, 
	graphs = TRUE, k = 0)
}
\arguments{
  \item{DATA}{original data to perform a PCA on.}
  \item{scale}{a boolean, vector, or string. See \code{\link{expo.scale}} for details.}
  \item{center}{a boolean, vector, or string. See \code{\link{expo.scale}} for details.}
  \item{DESIGN}{a design matrix to indicate if rows belong to groups.}
  \item{make_design_nominal}{a boolean. If TRUE (default), DESIGN is a vector that indicates groups (and will be dummy-coded). If FALSE, DESIGN is a dummy-coded matrix.}
  \item{graphs}{a boolean. If TRUE (default), graphs and plots are provided (via \code{\link{epGraphs}})}
  \item{k}{number of components to return.}
}
\details{
	\code{epPCA} performs principal components analysis on a data matrix. 
}
\value{
See \code{\link{corePCA}} for details on what is returned.
}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{corePCA}}, \code{\link{epMDS}}
}
\examples{
	data(words)
	pca.words.res <- epPCA(words$data)
}
\keyword{ multivariate }
