\name{hellingerSupplementaryColsPreProcessing}
\alias{hellingerSupplementaryColsPreProcessing}
\title{
Preprocessing for supplementary columns in Hellinger analyses.
}
\description{
Preprocessing for supplementary columns in Hellinger analyses.
}
\usage{
hellingerSupplementaryColsPreProcessing(SUP.DATA, W = NULL, M = NULL)
}
\arguments{
 \item{SUP.DATA}{A supplemental matrix that has the same number of rows as an active data set.}
  \item{W}{A vector or matrix of Weights. If none are provided, a default is computed.}
  \item{M}{A vector or matrix of Masses. If none are provided, a default is computed.}
}
\value{
	a matrix that has been preprocessed to project supplementary rows for Hellinger methods.
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }