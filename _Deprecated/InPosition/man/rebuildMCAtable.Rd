\name{rebuildMCAtable}
\alias{rebuildMCAtable}
\title{
	rebuildMCAtable: rebuild categorical table from the disjunctive table.
}
\description{
rebuildMCAtable takes the disjunctive table used in MCA and rebuilds a categorical form of it. This function is used for permutation tests when only a disjunctive table is available.
}
\usage{
rebuildMCAtable(DATA)
}
\arguments{
  \item{DATA}{Disjunctive coded data table}
}
\value{
A categorical data table is returned. It has the same structure as the disjunctive table in a format that can be permuted.
}
\author{
Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
