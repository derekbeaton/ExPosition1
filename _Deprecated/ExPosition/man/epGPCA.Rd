\name{epGPCA}
\alias{epGPCA}
\title{
epGPCA: Generalized Principal Components Analysis (GPCA) via ExPosition.
}
\description{
Generalized Principal Components Analysis (GPCA) via ExPosition.
}
\usage{
epGPCA(DATA, scale = TRUE, center = TRUE, DESIGN = NULL, make_design_nominal = TRUE, 
	masses = NULL, weights = NULL, graphs = TRUE, k = 0)
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
}
\details{
	\code{epGPCA} performs generalized principal components analysis. Essentially, a PCA with masses and weights for rows and columns, respectively. 
}
\value{
	See \code{\link{corePCA}} for details on what is returned. In addition to the values in \code{\link{corePCA}}:\cr
	\item{M}{a matrix (or vector, depending on size) of masses for the row items.}
	\item{W}{a matrix (or vector, depending on size) of weights for the column items.}	
}
\references{
Abdi, H., and Williams, L.J. (2010). Principal component analysis. \emph{Wiley Interdisciplinary Reviews: Computational Statistics}, 2, 433-459.\cr
Abdi, H. (2007). Singular Value Decomposition (SVD) and Generalized Singular Value Decomposition (GSVD). In N.J. Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics}.Thousand Oaks (CA): Sage. pp. 907-912.
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{corePCA}}, \code{\link{epPCA}}, \code{\link{epMDS}}
}
\examples{
	#this is for ExPosition's iris data
	data(ep.iris)
	gpca.iris.res <- epGPCA(ep.iris$data,DESIGN=ep.iris$design,make_design_nominal=FALSE)
}
\keyword{ multivariate }
