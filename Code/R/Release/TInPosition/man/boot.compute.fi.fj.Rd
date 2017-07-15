\name{boot.compute.fi.fj}
\alias{boot.compute.fi.fj}
\title{
Bootstrap computations for TInPosition.
}
\description{
Provides bootstrap projections for $fi and $fj from TExPosition methods.
}
\usage{
boot.compute.fi.fj(DATA, DESIGN, res)
}

\arguments{
  \item{DATA}{
The original data matrix to be bootstrapped. Rows will be bootstrapped and are assumed to be observations. Resampling will be constrained to within groups based on \code{DESIGN}.
}
  \item{DESIGN}{
A design matrix (in disjunctive coding). Required for \code{TExPosition} and \code{TInPosition} analyses.
}
  \item{res}{
of class \code{texpoOutput}. Results from one of the \code{\link{TExPosition}} methods (e.g., \code{\link{tepDICA}}, \code{\link{tepBADA}}),
}
}

\value{
\item{FBX}{a set of factor scores of the measures (columns, \code{$fj}) for the bootstrapped data.}
\item{FBY}{a set of factor scores of the groups (\code{$fi}) for the bootstrapped data.}
}
\author{
Derek Beaton
}
\keyword{ bootstrap }
