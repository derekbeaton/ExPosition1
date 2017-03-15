\name{makeNominalData}
\alias{makeNominalData}
\title{
makeNominalData
}
\description{
Transforms each column into measure-response columns with disjunctive (0/1) coding. If NA is found somewhere in matrix, barycentric recoding is peformed for the missing value(s).
}
\usage{
makeNominalData(datain)
}
\arguments{
	\item{datain}{a data matrix where the \emph{columns} will be recoded.}
}
\value{
	\item{dataout}{a transformed version of \emph{datain}.}
}

\author{
	Derek Beaton
}

\seealso{
\code{\link{epMCA}}
}
\examples{
	data(mca.wine)
	nominal.wine <- makeNominalData(mca.wine$data)
}

\keyword{ misc }
\keyword{ multivariate }
