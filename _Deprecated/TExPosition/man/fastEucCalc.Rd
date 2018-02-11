\name{fastEucCalc}
\alias{fastEucCalc}
\encoding{UTF-8}
\title{
fastEucCalc
}
\description{
Fast Euclidean distance calculations. 
}
\usage{
fastEucCalc(x, c)
}
\arguments{
  \item{x}{a set of points.}
  \item{c}{a set of centers.}
}
\details{
This function is especially useful for discriminant analyses. The distance from each point in \code{x} to each point in \code{c} is computed and returned as a \code{nrow(x)} x \code{nrow(c)} matrix.
}
\value{
	\item{a distance matrix}{Euclidean distances of each point to each center are returned.}
}
\author{
	Hervé Abdi, Derek Beaton
}
\keyword{ misc }
