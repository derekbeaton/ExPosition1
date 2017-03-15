\name{nominalCheck}
\alias{nominalCheck}
\title{
Checks if data are disjunctive.
}
\description{
Checks if data is in disjunctive (sometimes called complete binary) format. To be used with MCA (e.g., \code{\link{epMCA}}).
}
\usage{
nominalCheck(DATA)
}
\arguments{
  \item{DATA}{A data matrix to check. This should be 0/1 disjunctive coded. \code{nominalCheck} just checks to make sure it is complete.}
}
\value{
	If \code{DATA} are nominal, \code{DATA} is returned. If not, \code{\link{stop}} is called and execution halts.
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
\keyword{ multiple correspondence analysis }
