\name{pcaSupplementaryRowsPreProcessing}
\alias{pcaSupplementaryRowsPreProcessing}
\title{
Preprocessing for supplemental rows in PCA.
}
\description{
Preprocessing for supplemental rows in PCA.
}
\usage{
pcaSupplementaryRowsPreProcessing(SUP.DATA = NULL, center = TRUE, scale = TRUE, W = NULL)
}
\arguments{
  \item{SUP.DATA}{A supplemental matrix that has the same number of columns as an active data set.}
  \item{center}{The center from the active data. NULL will center \code{SUP.DATA} to itself.}
  \item{scale}{The scale factor from the active data. NULL will scale (z-score) \code{SUP.DATA} to itself.}
  \item{W}{Weights from the active data.}
}
\value{
	a matrix that has been preprocessed to project supplementary rows for PCA methods.
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }