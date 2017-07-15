\name{computeMW}
\alias{computeMW}
\title{
computeMW
}
\description{
Computes masses and weights for \code{\link{epGPCA}}.
}
\usage{
computeMW(DATA, masses = NULL, weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{original data; will be used to compute masses and weights if none are provided.}
  \item{masses}{a vector or (diagonal) matrix of masses for the row items. If NULL (default), masses are computed as 1/# of rows}
  \item{weights}{a vector or (diagonal) matrix of weights for the column items. If NULL (default), weights are computed as 1/# of columns}
}

\value{
Returns a list with the following items:\cr
	\item{M}{a diagonal matrix of masses (if too large, a vector is returned).}
	\item{W}{a diagonal matrix of weights (if too large, a vector is returned).}
}

\author{
	Derek Beaton
}

\seealso{
	\code{\link{epGPCA}}
}
\keyword{ misc }
\keyword{ multivariate }
