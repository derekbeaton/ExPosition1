\name{mdsTransform}
\alias{mdsTransform}
\title{
Transform data for MDS analysis.
}
\description{
Transform data for MDS analysis.
}
\usage{
mdsTransform(D, masses)
}
\arguments{
  \item{D}{A distance matrix}
  \item{masses}{A vector or matrix of masses (see \code{computeMW}).}
}
\value{
	\item{S}{a preprocessed matrix that can be decomposed.}
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
