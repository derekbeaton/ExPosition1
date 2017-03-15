\name{makeRowProfiles}
\alias{makeRowProfiles}
\title{
Preprocessing for CA-based analyses
}
\description{
This function performs all preprocessing steps required for Correspondence Analysis-based preprocessing.
}
\usage{
makeRowProfiles(X, weights = NULL, masses = NULL, hellinger = FALSE)
}
\arguments{
  \item{X}{Data matrix.}
  \item{weights}{optional. Weights to include in preprocessing.}
  \item{masses}{optional. Masses to include in preprocessing.}
  \item{hellinger}{a boolean. If TRUE, Hellinger preprocessing is used. Else, CA row profile is computed.}
}
\value{
Returns from \code{link{hellingerNorm}} or \code{\link{caNorm}}.
}
\author{
Derek Beaton
}