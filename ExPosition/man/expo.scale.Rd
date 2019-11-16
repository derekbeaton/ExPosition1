\name{expo.scale}
\alias{expo.scale}
\title{
Scaling functions for ExPosition.
}
\description{
\code{expo.scale} is a more elaborate, and complete, version of \code{\link{scale}}. Several text options are available, but more importantly, the center and scale factors are \emph{always} returned.
}
\usage{
expo.scale(DATA, center = TRUE, scale = TRUE)
}
\arguments{
  \item{DATA}{Data to center, scale, or both.}
  \item{center}{boolean, or (numeric) vector. If boolean or vector, it works just as \code{\link{scale}}.}
  \item{scale}{boolean, text, or (numeric) vector. If boolean or vector, it works just as \code{\link{scale}}. The following text options are available: 'z': z-score normalization, 'sd': standard deviation normalization, 'rms': root mean square normalization, 'ss1': sum of squares (of columns) equals 1 normalization.}
}

\value{
	A data matrix that is scaled with the following \code{attributes} (see \code{\link{scale}}):\cr
	\item{$`scaled:center`}{The center of the data. If no center is provided, all 0s will be returned.}
	\item{$`scaled:scale`}{The scale factor of the data. If no scale is provided, all 1s will be returned.}
}
\author{
Derek Beaton
}
\keyword{ misc }