\name{mpTableCheck}
\alias{mpTableCheck}
\title{Table Check for MExPosition
}
\description{
MExPosition's table check function. Calls into ExPosition's \code{\link{designCheck}}.
}
\usage{
mpTableCheck(data, table = NULL, make_table_nominal = TRUE)
}
\arguments{
  \item{data}{original data that should be matched to a design matrix}
  \item{table}{a column vector with levels for observations or a dummy-coded matrix}
  \item{make_table_nominal}{a boolean. Will make DESIGN nominal if TRUE (default).}
}
\details{
  Execution stops if:\cr
  1. design has only 1 column (group), or
	2. A column of the table has too few column-table assignments, or
  3. A column of the table has too many column-table assignments
}
\value{
\item{table}{dummy-coded design matrix}
}

\author{
Derek Beaton and Cherise R. Chin Fatt
}
\keyword{ misc }
