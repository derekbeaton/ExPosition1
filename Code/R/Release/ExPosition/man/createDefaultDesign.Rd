\name{createDefaultDesign}
\alias{createDefaultDesign}
\title{
createDefaultDesign
}
\description{
Creates a default design matrix, wherein all observations (i.e., row items) are in the same group.
}
\usage{
createDefaultDesign(DATA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{original data that requires a design matrix}
}

\value{
	\item{DESIGN}{a column-vector matrix to indicate that all observations are in the same group.}
}

\author{
Derek Beaton
}
\keyword{ misc }
