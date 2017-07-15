\name{contingency.data.break}
\alias{contingency.data.break}
\title{
Bootstrap or permutation resampling for contingency tables
}
\description{
Bootstrap or permutation resampling for contingency tables. More specifically, for correspondence analysis (\code{\link{epCA}}).
}
\usage{
contingency.data.break(DATA, boot = FALSE)
}
\arguments{
  \item{DATA}{
A contingency table to resample.
}
  \item{boot}{a boolean. If TRUE, use bootstrap (resample with replacement) resampling. If FALSE, use permutation (resample with no replacement).
}
}
\value{
A resampled contingency table.
}

\author{
Joseph Dunlop and Derek Beaton
}
\seealso{
\code{\link{epCA}}, \code{\link{epCA.inference.battery}}
}
\examples{
	data(authors)
	boot.authors <- contingency.data.break(authors$ca$data,boot=TRUE)
	perm.authors <- contingency.data.break(authors$ca$data)	
}
\keyword{ bootstrap }
\keyword{ permutation }