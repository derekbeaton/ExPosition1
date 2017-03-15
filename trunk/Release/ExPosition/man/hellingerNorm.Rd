\name{hellingerNorm}
\alias{hellingerNorm}
\encoding{UTF-8}
\title{
Hellinger version of CA preprocessing
}
\description{
Performs all steps required for Hellinger form of CA processing (row profile approach).
}
\usage{
hellingerNorm(X, X_dimensions, colTotal, rowTotal, grandTotal, 
	weights = NULL, masses = NULL)
}
\arguments{
  \item{X}{Data matrix}
  \item{X_dimensions}{The dimensions of X in a vector of length 2 (rows, columns). See \code{\link{dim}}}
  \item{colTotal}{Vector of column sums.}
  \item{rowTotal}{Vector of row sums.}
  \item{grandTotal}{Grand total of X}
  \item{weights}{Optional weights to include for the columns.}
  \item{masses}{Optional masses to include for the rows.}
 }
\value{
\item{rowCenter}{The barycenter of X.}
\item{masses}{Masses to be used for the GSVD.}
\item{weights}{Weights to be used for the GSVD.}
\item{rowProfiles}{The row profiles of X.}
\item{deviations}{Deviations of row profiles from \code{rowCenter}.}
}
\author{
Derek Beaton and Hervé Abdi
}