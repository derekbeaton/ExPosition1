\name{designCheck}
\alias{designCheck}
\title{
designCheck
}
\description{
Checks and/or creates a dummy-coded design matrix.
}
\usage{
designCheck(DATA, DESIGN = NULL, make_design_nominal = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DATA}{original data that should be matched to a design matrix}
  \item{DESIGN}{a column vector with levels for observations or a dummy-coded matrix}
  \item{make_design_nominal}{a boolean. Will make DESIGN nominal if TRUE (default).}
}
\details{
Returns a properly formatted, dummy-coded (or disjunctive coding) design matrix.
}
\value{
\item{DESIGN}{dummy-coded design matrix}
}
\author{
Derek Beaton
}

\examples{
	data <- iris[,c(1:4)]
	design <- as.matrix(iris[,c('Species')])
	iris.design <- designCheck(data,DESIGN=design,make_design_nominal=TRUE)
}
\keyword{ misc }
