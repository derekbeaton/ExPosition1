\name{mdsSupplementalElementsPreProcessing}
\alias{mdsSupplementalElementsPreProcessing}
\title{
MDS preprocessing
}
\description{
Preprocessing of supplemental data for MDS analyses.
}
\usage{
mdsSupplementalElementsPreProcessing(SUP.DATA = NULL, D = NULL, M = NULL)
}
\arguments{
  \item{SUP.DATA}{A supplementary data matrix.}
  \item{D}{The original (active) distance matrix that \code{SUP.DATA} is supplementary to.}
  \item{M}{masses from the original (active) analysis for \code{D}.}
}
\value{
	a matrix that is preprocessed for supplementary projection in MDS.
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
