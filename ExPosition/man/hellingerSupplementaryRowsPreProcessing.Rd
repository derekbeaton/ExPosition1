% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hellingerSupplementaryRowsPreProcessing.R
\name{hellingerSupplementaryRowsPreProcessing}
\alias{hellingerSupplementaryRowsPreProcessing}
\title{Preprocessing for supplementary rows in Hellinger analyses.}
\usage{
hellingerSupplementaryRowsPreProcessing(SUP.DATA, center = NULL)
}
\arguments{
\item{SUP.DATA}{A supplemental matrix that has the same number of rows as an
active data set.}

\item{center}{The center from the active data. NULL will center
\code{SUP.DATA} to itself.}
}
\value{
a matrix that has been preprocessed to project supplementary columns
for Hellinger methods.
}
\description{
Preprocessing for supplementary rows in Hellinger analyses.
}
\author{
Derek Beaton
}
\keyword{misc}
\keyword{multivariate}
