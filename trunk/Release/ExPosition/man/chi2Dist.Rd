\name{chi2Dist}
\alias{chi2Dist}
\encoding{UTF-8}
\title{
Chi-square Distance computation
}
\description{
Performs a chi-square distance. Primarily used for \code{\link{epMDS}}.
}
\usage{
chi2Dist(X)
}
\arguments{
  \item{X}{Compute chi-square distances between row items.}
}
\value{
\item{D}{Distance matrix for \code{\link{epMDS}} analysis.}
\item{MW}{a list of masses and weights. Weights not used in MDS.}
}
\author{
Hervé Abdi
}
\keyword{ misc }
\keyword{ multivariate }
