\name{fii2fi}
\alias{fii2fi}
\encoding{UTF-8}
\title{
fii2fi: individuals to centers
}
\description{
All computations between individual factor scores (fii) and group factor scores (fi).
}
\usage{
fii2fi(DESIGN, fii, fi)
}
\arguments{
  \item{DESIGN}{a dummy-coded design matrix}
  \item{fii}{a set of factor scores for individuals (rows)}
  \item{fi}{a set of factor scores for rows}
}
\value{
A list of values containing:
\item{distances}{Euclidean distances of all rows to each category center}
\item{assignments}{an assignment matrix (similar to DESIGN) where each individual is assigned to the closest category center}
\item{confusion}{a confusion matrix of how many items are assigned (and mis-assigned) to each category}
}
\author{
Hervé Abdi, Derek Beaton
}
\keyword{ misc }
\keyword{ multivariate }
