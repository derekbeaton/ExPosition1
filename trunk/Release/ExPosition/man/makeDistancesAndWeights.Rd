\name{makeDistancesAndWeights}
\alias{makeDistancesAndWeights}
\title{
Makes distances and weights for MDS analyses (see \code{\link{epMDS}}).
}
\description{
	Makes distances and weights for MDS analyses (see \code{\link{epMDS}}).
}
\usage{
makeDistancesAndWeights(DATA, method = "euclidean", masses = NULL)
}
\arguments{
  \item{DATA}{A data matrix to compute distances between row items.}
  \item{method}{which distance metric should be used. \code{method} matches \code{\link{dist}}; Two additional distances are avaialble: "correlation" and "chi2". For "chi2" see \code{\link{chi2Dist}}. Default is "euclidean".}
  \item{masses}{a diagonal matrix (or vector) that contains the masses (for the row items).}
}
\value{
	\item{D}{Distance matrix for analysis}
	\item{MW}{a list item with masses and weights. Weights are not used in \code{\link{epMDS}}.}	
}
\author{
Derek Beaton
}
\seealso{
	\code{link{computeMW}},	\code{link{epMDS}},	\code{link{coreMDS}}
}
\keyword{ misc }
\keyword{ multivariate }
