\name{caSupplementalElementsPreProcessing}
\alias{caSupplementalElementsPreProcessing}
\title{
Correspondence Analysis preprocessing. 
}
\description{
CA preprocessing for data. Can be performed on rows or columns of your data. This is a row-profile normalization.
}
\usage{
caSupplementalElementsPreProcessing(SUP.DATA)
}
\arguments{
  \item{SUP.DATA}{Data that will be supplemental. Row profile normalization is used. For supplemental rows use \code{t(SUP.DATA)}.}
}
\value{
	returns a matrix that is preprocessed for supplemental projections.
}
\author{
Derek Beaton
}

\seealso{
	\code{\link{mdsSupplementalElementsPreProcessing}}, \code{\link{pcaSupplementaryColsPreProcessing}}, \code{\link{pcaSupplementaryRowsPreProcessing}}, \code{\link{hellingerSupplementaryColsPreProcessing}}, \code{\link{hellingerSupplementaryRowsPreProcessing}}, \code{\link{supplementaryCols}}, \code{\link{supplementaryRows}}, \code{\link{supplementalProjection}}, \code{\link{rowNorms}}
}
\keyword{ misc }
\keyword{ multivariate }
